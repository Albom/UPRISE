
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


Dim Shared As Double razr(0 To 350)
Dim Shared As Double razr7(0 To 350*7)
Dim Shared As Double alt(0 To 350)
Dim As String razr_filename
Dim As Integer file

'''==============================================

Open Err For Output As #1


file = FreeFile()
Open "config.dat" For Input As #file
Input #file, razr_filename
Close #file

If razr_load(razr_filename, @razr(0), 330) = 0 Then ' загрузка разрядника
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf

For h = 0 To 330
	alt(h) = h
Next h

For h = 0 To 330*7
	razr7(h) = array_linear_d(CDbl(h)/7.0, @alt(0), @razr(0), 330)
Next h

/'
Open "r4.txt" For Output As #33

For h = 0 To 300
	' вспомогательные локальные переменные (видны только в цикле)
	Dim As Integer l1, l2 ' индексы
	Dim As Double  r1, r2 ' значения коэффициента передачи

	Print #33,Using "### ####.### "; h; seans1c_alt(h);

	l1 = h*15

	For tau = 0 To 18 ' по задержке

		Dim As Double ppp = 0

		l2 = l1+tau*7

		If l1\15 < 300 Then
			r1 = razr7(l1)
		Else
			r1 = 1.0
		EndIf

		If l2\15 < 300 Then
			r2 = razr7(l2)
		Else
			r2 = 1.0
		EndIf

		If r1*r2 > 1e-6 Then
			ppp = Sqr( r1*r2 )
		EndIf

		If tau = 0 Or (  (tau >= 7) And (tau <=12) ) Then
			Print #33, Using " ###.###"; ppp;
		EndIf

	Next tau

	Print #33,
Next h

Close #33
break

'/

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
'Print #file, "Параметр трапецеидального суммирования: "; partrap
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

/'

	' учёт разрядника
'	For h = h_start To h_end-1 ' по высоте
	For h = 19 To h_end-1 ' по высоте
		' вспомогательные локальные переменные (видны только в цикле)
		Dim As Integer l1, l2 ' индексы
		Dim As Double  r1, r2 ' значения коэффициента передачи
		Dim As Integer offset = 10

		l1 = ((h+offset)*15)\7
		For tau = 0 To 12 ' по задержке

			l2 = l1-tau*7
			Print #1, h, l1, l2, tau
Sleep 500

			If l1 < 300*7 Then
				r1 = razr7(l1)
			Else
				r1 = 1.0
			EndIf

			If l2 < 300*7 And l2 >= 0 Then
				r2 = razr7(l2)
			Else
				r2 = 1.0
			EndIf

			If r1*r2 > 1e-6 Then
				as_file_in.acf[h].rc(tau) /= Sqr( r1*r2 )
				as_file_in.acf[h].rs(tau) /= Sqr( r1*r2 )
			Else
				as_file_in.acf[h].rc(tau) = 0
				as_file_in.acf[h].rs(tau) = 0
			EndIf

		Next tau

	Next h

	'break


'/


	For h = h_start To h_end-1 ' по высоте
		as_file_out.acf[h].pShort = 0
	Next h



	' 1) для косинусной составляющей

	For h = 0 To as_file_in.nh-1
		For tau = 0 To 18
			as_file_out.acf[h].rc(tau) = as_file_in.acf[h].rc(tau)
		Next tau
	Next h

	/'
	' 2) для синусной составляющей

	For h = 0 To as_file_in.nh-1

		If h >= 18+partrap And h <= as_file_in.nh-partrap-1 Then

		For tau = 0 To 18

			as_file_out.acf[h].rs(tau) = 0
			For z = h-tau-partrap To h+partrap ' по высоте
				as_file_out.acf[h].rs(tau) += as_file_in.acf[z].rs(tau)
			Next z

			If is_divide = 1 Then
				as_file_out.acf[h].rs(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
			EndIf

		Next tau

		Else

			For tau = 0 To 18
				as_file_out.acf[h].rs(tau) = 0
			Next tau

		EndIf

	Next h
'/



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
