
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"

#Include "fbgfx.bi"			'Подключение графической библиотеки


'''==============================================

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf


'''==============================================

Dim As Integer partrap

Dim As Integer wnd
Dim As Integer poly_degree = 3

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

Dim As Integer is_divide
Dim As Integer isPower

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
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Approximate trapezoidal - программа высотной коррекции данных"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays


Dim As Integer h_start_poly
Print
'Input "Введите параметр трапецеидального суммирования: ", partrap
Input "Введите индекс начальной высоты для аппроксимации: ", h_start_poly
Input "Введите ширину окна для аппроксимации: ", wnd
Input "Аппроксимировать только мощность? (0 - нет, 1 - да): ", isPower
Print

partrap = 0
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

	For h = h_start To h_end-1

		as_file_out.acf[h].n = h
		as_file_out.acf[h].h = as_file_in.acf[h].h

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


	file = FreeFile()
	Open SEANS_DIR_OUT+DirectoryOutput+"/step2/Init"+DirectoryOutput+"."+ext For Output As #file
	For h = 0 To 679
		For tau = 0 To 18
			Print #file, Using "##.####^^^ "; as_file_in.acf[h].rc(tau);
		Next tau
		Print #file,
	Next h
	Close #file


	Dim As Double rTmp(0 To 679, 0 To 18)
	Dim As Double in_x(0 To 679)
	Dim As Double in_y(0 To 679)
	Dim As Double coeff(0 To 100)


	For h = 0 To 679
		For tau = 0 To 18
			rTmp(h, tau) = as_file_in.acf[h].rc(tau)
		Next tau
	Next h

	If isPower <> 0 Then

		tau = 0

		For h = h_start_poly To 679-1-wnd/2

			Dim As Integer y = 0
			For z As Integer = -wnd/2 To wnd/2
				If h+z >= 0 And h+z <= h_end Then
					in_x(y) = as_file_in.acf[h+z].h
					in_y(y) = as_file_in.acf[h+z].rc(tau)
					y += 1
				EndIf
			Next z

			approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), poly_degree)

			rTmp(h, tau) = 0
			For k As Integer = 0 To poly_degree
				rTmp(h, tau) += coeff(k)*( as_file_out.acf[h].h )^k
			Next k

		Next h

	Else

		For tau = 0 To 18

			For h = h_start_poly To 679-1-wnd/2

				Dim As Integer y = 0
				For z As Integer = -wnd/2 To wnd/2
					If h+z >= 0 And h+z <= h_end Then
						in_x(y) = as_file_in.acf[h+z].h
						in_y(y) = as_file_in.acf[h+z].rc(tau)
						y += 1
					EndIf
				Next z

				approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), poly_degree)

				rTmp(h, tau) = 0
				For k As Integer = 0 To poly_degree
					rTmp(h, tau) += coeff(k)*( as_file_out.acf[h].h )^k
				Next k

			Next h

		Next tau

	EndIf


	file = FreeFile()
	Open SEANS_DIR_OUT+DirectoryOutput+"/step2/Poly"+DirectoryOutput+"."+ext For Output As #file
	For h = 0 To 679
		For tau = 0 To 18
			Print #file, Using "##.####^^^ "; rTmp(h, tau);
		Next tau
		Print #file,
	Next h
	Close #file

	' 1) для косинусной составляющей

	For h = 0 To as_file_in.nh-1

		If h >= 18+partrap And h <= as_file_in.nh-partrap-1 Then

			For tau = 0 To 18

				as_file_out.acf[h].rc(tau) = 0
				For z = h-tau-partrap To h+partrap ' по высоте
					as_file_out.acf[h].rc(tau) += rTmp(z, tau)'  as_file_in.acf[z].rc(tau)
				Next z

				If is_divide = 1 Then
					as_file_out.acf[h].rc(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
				EndIf

			Next tau

		Else

			For tau = 0 To 18
				as_file_out.acf[h].rc(tau) = 0
			Next tau

		EndIf

	Next h

	file = FreeFile()
	Open SEANS_DIR_OUT+DirectoryOutput+"/step2/Trap"+DirectoryOutput+"."+ext For Output As #file
	For h = 0 To 679
		For tau = 0 To 18
			Print #file, Using "##.####^^^ "; as_file_out.acf[h].rc(tau);
		Next tau
		Print #file,
	Next h
	Close #file


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

	' Запись корректированной мощности и отношения с/ш, расчёт и запись отношения с/ш

	For h = 0 To as_file_in.nh-1

		as_file_out.acf[h].pcorr = as_file_in.acf[h].pcorr
		as_file_out.acf[h].qcorr = as_file_in.acf[h].qcorr

		as_file_out.acf[h].q = as_file_out.acf[h].rc(0)/as_file_out.rnc(0)


	Next h

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext



	as_file_save(filename, @as_file_out)



	DeAllocate(as_file_out.acf)   ' освобождаем память с АКФ
	'	DeAllocate(as_file_out.param) ' освобождаем память с параметрами

	as_file_close(@as_file_in)

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput + "/step2/" + "profileTrap.txt" For Output As #file
	For h = 0 To 679
		Print #file, Using "####.# ###"; seans2_altL(h); 0
	Next h
	Close #file

Next t

Print_process_percent(1000)
Print "OK"


Print
Print "OK"
break

'''==============================================
