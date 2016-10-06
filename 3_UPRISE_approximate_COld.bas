
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"

#Include "window9.bi"

#Include "fbgfx.bi"			'Подключение графической библиотеки


'''==============================================

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf


'''==============================================

Dim As Integer partrap

Dim As Integer i, j

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_OUT = "./out/"
Dim As String filename
Dim As String ext

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer seans_num_in
Dim As Integer seans_current
Dim As Integer seans_num_out

Dim As Integer t, tau, h
Dim As Integer z ' номер текущей высоты

Dim As as_file_struct	as_file_in
Dim As as_file_struct 	as_file_out


Dim Shared As Double razr(0 To 359, 0 To 6)
Dim Shared As Double alt(0 To 359)
Dim As String razr_filename
Dim As Integer file

'''==============================================

Open Err For Output As #1


file = FreeFile()
Open "config_COld.dat" For Input As #file
Input #file, razr_filename
Close #file

Open razr_filename For Input As #file
For h = 0 To 359
	For tau = 0 To 6
		Input #file, razr(h, tau)
	Next tau
Next h
Close #file




SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Approximate COld - программа высотной коррекции данных, полученных в 4 режиме"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays





DirectoryOutput = ""

For i = 0 To d_ndays-1
	If date_valid(d_day, d_month, d_year) = 1 Then

		If d_day < 10 Then
			s_day = "0"+Str(d_day)
		Else
			s_day = Str(d_day)
		EndIf

		If d_month < 10 Then
			s_month = "0"+Str(d_month)
		Else
			s_month = Str(d_month)
		EndIf

		s_year = Mid(Str(d_year), 3, 2)

		directory = s_day+s_month+s_year

		date_next(@d_day, @d_month, @d_year)

		If i = 0 Then
			DirectoryOutput += directory + "-"
		EndIf
	EndIf
Next i
DirectoryOutput += directory


Print
Print "Подсчёт количества входных файлов... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step1")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"



file = FreeFile()
Open SEANS_DIR_OUT +DirectoryOutput+"/step2/input.txt" For Output As #file
Close #file



Print "Суммирование по высоте... ";

seans_num_out = seans_num_in

DeleteDir(SEANS_DIR_OUT + DirectoryOutput+"/step2", /'FOF_ALLOWUNDO Or '/FOF_NOCONFIRMATION Or FOF_SILENT)
MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step2")

For t = 0 To seans_num_out-1 ' по времени

	Print_process_percent((t*100)/seans_num_out)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step1/AS"+DirectoryOutput+"."+ext

	If as_file_load(filename, @as_file_in) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
		End
	EndIf

	as_file_out.filename = as_file_in.filename
	as_file_out.date_    = as_file_in.date_
	as_file_out.time_    = as_file_in.time_
	as_file_out.nseans   = as_file_in.nseans
	as_file_out.tnak     = as_file_in.tnak

	For tau = 0 To 18  ' по задержке
		as_file_out.rnc(tau) = as_file_in.rnc(tau)
		as_file_out.rns(tau) = as_file_in.rns(tau)
	Next tau

	as_file_out.nh = as_file_in.nh

	as_file_out.acf = Callocate(as_file_out.nh, SizeOf(acf_struct) )
	If as_file_out.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	Dim As Integer h_start = 0
	Dim As Integer h_end = 360

	For h = h_start To h_end-1

		as_file_out.acf[h].n = h
		as_file_out.acf[h].h = as_file_in.acf[h].h

	Next h



	' Вычитание шума
	For h = h_start To h_end-1
		For tau = 0 To 18
			as_file_in.acf[h].rc(tau) -= as_file_in.rnc(tau)
		Next tau
	Next h



	' учёт разрядника

	For h = 19 To h_end-1 ' по высоте

		If razr(h, 0) > 1e-3 Then
			as_file_in.acf[h].rc(0) /= razr(h, 0)
			as_file_in.acf[h].rs(0) /= razr(h, 0)
		Else
			as_file_in.acf[h].rc(0) = 0
			as_file_in.acf[h].rs(0) = 0
		EndIf

		For tau = 7 To 12 ' по задержке

			If razr(h, tau) > 1e-3 Then
				as_file_in.acf[h].rc(tau) /= razr(h, tau)
				as_file_in.acf[h].rs(tau) /= razr(h, tau)
			Else
				as_file_in.acf[h].rc(tau) = 0
				as_file_in.acf[h].rs(tau) = 0
			EndIf

		Next tau

	Next h




	For h = h_start To h_end-1 ' по высоте
		as_file_out.acf[h].pShort = 0
	Next h



	' 1) для косинусной составляющей
/'
	For h = 0 To as_file_in.nh-1
		For tau = 0 To 18
			If h + tau < 359 Then
				as_file_out.acf[h].rc(tau) = as_file_in.acf[h+tau].rc(tau)
			Else
				as_file_out.acf[h].rc(tau) = 0
			EndIf
		Next tau
Next h
'/


	For h = 0 To as_file_in.nh-1
		For tau = 0 To 18
				as_file_out.acf[h].rc(tau) = as_file_in.acf[h].rc(tau)
		Next tau
	Next h



	' 2) для синусной составляющей

	For h = 0 To as_file_in.nh-1
		For tau = 0 To 18
			If h - tau > 0 Then
				as_file_out.acf[h].rs(tau) = as_file_in.acf[h-tau].rs(tau)
			Else
				as_file_out.acf[h].rs(tau) = 0
			EndIf
	Next tau
	Next h


	' Запись отношения с/ш

	For h = 0 To as_file_in.nh-1
		as_file_out.acf[h].q = as_file_out.acf[h].rc(0)/as_file_out.rnc(0)
	Next h


	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext



	as_file_save(filename, @as_file_out)



	DeAllocate(as_file_out.acf)   ' освобождаем память с АКФ
	'	DeAllocate(as_file_out.param) ' освобождаем память с параметрами

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"


Print
Print "OK"
break

'''==============================================
