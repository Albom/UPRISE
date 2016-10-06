
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


Dim As Integer h_start = 61'117
Dim As Integer h_end   = 250'185
Dim As Integer h_num = h_end - h_start

Dim As Double in_x(0 To 680)
Dim As Double in_y(0 To 680)

Dim As Double coeff(0 To 100)

Dim As Integer h_appr

Dim As Integer wnd  

'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Cls
Color 11
Print "UPRISE version 1.0 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print 
Color 7
Print "Approximate polynomial - программа высотной коррекции данных"
Print "автор: Александр Богомаз, н.с. Института ионосферы"
Print

Color 15


Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

Print
Input "Введите номер высоты, начиная с которой производить аппроксимацию: ", h_appr
Input "Введите ширину окна аппроксимации (например, 4 или 6): ", wnd   


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

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"




Print "Суммирование по высоте... ";

seans_num_out = seans_num_in

MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")

For t = 0 To seans_num_out-1 ' по времени

	Print_process_percent((t*100)/seans_num_out)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext

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


	as_file_out.nh = h_num


	as_file_out.acf = Callocate(as_file_out.nh, SizeOf(acf_struct) )
	If as_file_out.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	as_file_out.param = Callocate(as_file_out.nh, SizeOf(param_struct) )
	If as_file_out.param = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf



	h_out = 0
	For h = h_start To h_end-1

		as_file_out.acf[h_out].n = h_out+1
		as_file_out.acf[h_out].h = as_file_in.acf[h].h

		h_out += 1

	Next h


	Dim As Double  pmax = -1e200 ' значение максимальной мощности
	Dim As Integer hmax			  ' высота максимума
	For h = h_start To h_end-1
		If as_file_in.acf[h].rc(0) > pmax Then
			pmax = as_file_in.acf[h].rc(0)
			hmax = h
		EndIf
	Next h


	' 1) для косинусной составляющей

	Dim As Integer poly_degree = 3

	For tau = 0 To 18

		h_out = 0
		For h = h_start To h_end-1

'			wnd = tau + 10
'			If as_file_in.acf[h].rc(tau) < 0.7*pmax And h > hmax Then
'				wnd *= 2
'			EndIf

			Dim As Integer y = 0
			For z As Integer = -wnd/2 To wnd/2
				in_x(y) = as_file_in.acf[h+z].h
				in_y(y) = as_file_in.acf[h+z].rc(tau)
				y += 1
			Next z

			If h >= h_appr Then
				approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), poly_degree)

				as_file_out.acf[h_out].rc(tau) = 0
				For k As Integer = 0 To poly_degree
					as_file_out.acf[h_out].rc(tau) += coeff(k)*( as_file_out.acf[h_out].h  - 4.583*tau/2  )^k
				Next k
			Else
				as_file_out.acf[h_out].rc(tau) = array_linear_d(as_file_out.acf[h_out].h  - 4.583*tau/2, @in_x(0), @in_y(0), y)
			EndIf

			h_out += 1

		Next h

	Next tau



	' 2) для синусной составляющей


	For tau = 0 To 18

		h_out = 0
		For h = h_start To h_end-1

			Dim As Integer y = 0
			For z As Integer = -wnd/2 To wnd/2
				in_x(y) = as_file_in.acf[h+z].h
				in_y(y) = as_file_in.acf[h+z].rs(tau)
				y += 1
			Next z

			If h >= h_appr Then
				approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), poly_degree)

				as_file_out.acf[h_out].rs(tau) = 0
				For k As Integer = 0 To poly_degree
					as_file_out.acf[h_out].rs(tau) += coeff(k)*( as_file_out.acf[h_out].h  - 4.583*tau/2  )^k
				Next k
			Else
				as_file_out.acf[h_out].rs(tau) = array_linear_d(as_file_out.acf[h_out].h  - 4.583*tau/2, @in_x(0), @in_y(0), y)
			EndIf

			h_out += 1

		Next h

	Next tau


	' Запись корректированной мощности и отношения с/ш, запись отношения с/ш

	h_out = 0
	For h = h_start To h_end-1

		as_file_out.acf[h_out].pcorr = as_file_in.acf[h].pcorr
		as_file_out.acf[h_out].qcorr = as_file_in.acf[h].qcorr

		as_file_out.acf[h_out].q = as_file_in.acf[h].q

		h_out += 1

	Next h

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step3/AS"+DirectoryOutput+"."+ext



	as_file_save(filename, @as_file_out)



	DeAllocate(as_file_out.acf)   ' освобождаем память с АКФ
	DeAllocate(as_file_out.param) ' освобождаем память с параметрами

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"


Print
Print "OK"
break

'''==============================================
