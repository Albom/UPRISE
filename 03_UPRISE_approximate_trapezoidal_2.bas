
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами

#Include "crt/stdlib.bi"

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

Dim As Integer h_out

Dim As Integer is_divide

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename
Dim As Integer file

'''==============================================

file = FreeFile()
Open "config.dat" For Input As #file
Input #file, razr_filename
Close #file

If razr_load(razr_filename, @razr(0), 330) = 0 Then ' загрузка разрядника
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf


SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Cls
Color 11
Print "UPRISE version 1.1 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Approximate trapezoidal - программа высотной коррекции данных"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

Print
Input "Введите параметр трапецеидального суммирования: ", partrap
Print

'partrap = 0
'Input "Делить на количество слагаемых? (0 - нет, 1 - да): ", is_divide
is_divide=1

If (is_divide <> 0) And (is_divide <> 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf



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
Print #file, "Параметр трапецеидального суммирования: "; partrap
Close #file



Print "Суммирование по высоте... ";

seans_num_out = seans_num_in

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

	as_file_out.nh = as_file_in.nh 'as_file_in.nh-partrap-18-partrap

	as_file_out.acf = Callocate(as_file_out.nh, SizeOf(acf_struct) )
	If as_file_out.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	Dim As Integer h_start = 0
	Dim As Integer h_end = 680

	h_out = 0
	For h = h_start To h_end-1

		as_file_out.acf[h_out].n = h_out
		as_file_out.acf[h_out].h = as_file_in.acf[h].h

		h_out += 1

	Next h



	' умножить на 2
	For h = h_start To h_end-1
		For tau = 0 To 18
			as_file_in.acf[h].rc(tau) *= 2
			as_file_in.acf[h].rs(tau) *= 2
		Next tau
	Next h



	' Вычитание шума
	For h = h_start To h_end-1
		For tau = 0 To 18
			as_file_in.acf[h].rc(tau) -= as_file_in.rnc(tau)
			as_file_in.acf[h].rs(tau) -= as_file_in.rns(tau)
		Next tau
	Next h

	Dim As Double pNShort = 0
	For h = 500 To 599
		pNShort += as_file_in.acf[h].pShort
	Next h
	pNShort /= 100

	For h = h_start To h_end-1
		as_file_in.acf[h].pShort -= pNShort
	Next h




	' учёт разрядника
	For h = h_start To h_end-1 ' по высоте
		' вспомогательные локальные переменные (видны только в цикле)
		Dim As Integer l1, l2 ' индексы
		Dim As Double  r1, r2 ' значения коэффициента передачи

		l1 = h
		For tau = 0 To 18 ' по задержке

			l2 = h+tau

			If l1 < 300 Then
				r1 = razr(l1)
			Else
				r1 = 1.0
			EndIf

			If l2 < 300 Then
				r2 = razr(l2)
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


	' Вычитание шума
	For h = h_start To h_end-1
		For tau = 0 To 18
			as_file_in.acf[h].rc(tau) -= as_file_in.rnc(tau)
			as_file_in.acf[h].rs(tau) -= as_file_in.rns(tau)
		Next tau
	Next h



	' учёт разрядника для профиля мощности по короткому импульсу
	For h = h_start To h_end-1-12 ' по высоте

		Dim As Double r ' значение коэффициента передачи

		If h < 300-12 Then
			r = razr(h+12)
		Else
			r = 1.0
		EndIf

		If r > 1e-6 Then
			as_file_in.acf[h].pShort /= r
		Else
			as_file_in.acf[h].pShort = 0
		EndIf

	Next h


	For h = h_start To h_end-1 ' по высоте
		as_file_out.acf[h].pShort = as_file_in.acf[h].pShort
	Next h



	' 1) для косинусной составляющей
	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1

		For tau = 0 To 18

			as_file_out.acf[h_out].rc(tau) = 0
			For z = h-tau-partrap To h+partrap ' по высоте
				as_file_out.acf[h_out].rc(tau) += as_file_in.acf[z].rc(tau)
			Next z

			If is_divide = 1 Then
				as_file_out.acf[h_out].rc(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
			EndIf

		Next tau

		h_out += 1

	Next h

	' 2) для синусной составляющей
	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1
		For tau = 0 To 18

			as_file_out.acf[h_out].rs(tau) = 0
			For z = h-tau-partrap To h+partrap ' по высоте
				as_file_out.acf[h_out].rs(tau) += as_file_in.acf[z].rs(tau)
			Next z

			If is_divide = 1 Then
				as_file_out.acf[h_out].rs(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
			EndIf

		Next tau

		h_out += 1
	Next h

	' Запись корректированной мощности и отношения с/ш, расчёт и запись отношения с/ш

	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1

		as_file_out.acf[h_out].pcorr = as_file_in.acf[h].pcorr
		as_file_out.acf[h_out].qcorr = as_file_in.acf[h].qcorr

		as_file_out.acf[h_out].q = as_file_out.acf[h_out].rc(0)/as_file_out.rnc(0)

		h_out += 1

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
