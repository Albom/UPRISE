
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "fbgfx.bi"			'Подключение графической библиотеки

#Include "windows.bi"
#Include "file.bi"
#Include "dir.bi"

#Include "window9.bi"


'''==============================================

Type dat_all_struct

	Dim	acf(0 To 18)	As Double
	Dim	q					As Double

	Dim	d_c				As Double
	Dim	ti_c				As Double	' температура ионов
	Dim	te_c				As Double	' температура электронов

	Dim	ti_start			As Double
	Dim	te_start			As Double

	Dim	ti_end			As Double
	Dim	te_end			As Double

	Dim	nTau				As Integer

End Type

'''==============================================

Const As Double delta_tau = 30.555
Const As Double pulse_length = 663.0
Const	As Integer num_point_acf = 22
Const	As Integer NUM_H = 680

'''==============================================

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim Shared As String SEANS_DIR_OUT
Dim As String filename
Dim As String ext

Dim Shared As String directory, DirectoryOutput

Dim Shared As Integer seans_num

Dim As Integer t, tau, h, lag, z

Dim Shared As Integer Hmin, Hmax, Hstep

Dim As as_file_struct	as_file_in

Dim Shared As Double  acf_filter(0 To 100) ' АКФ фильтра

Dim As Integer file

Dim Shared As Integer Hkm(0 To NUM_H-1)

Dim Shared  As String LIBRARY_PATH
Dim Shared As FILE Ptr library_file
Dim Shared As Integer temperatures_len
Dim Shared As Integer temperatures(0 To 200000) ' размер должен быть не менее 2*temperatures_len


Dim Shared As Integer Config_step_ti_1,  Config_step_te_1
Dim Shared As Integer Config_step_ti_2,  Config_step_te_2
Dim Shared As Integer Config_step_ti_3,  Config_step_te_3

Dim Shared As Integer Config_range_ti_2, Config_range_te_2
Dim Shared As Integer Config_range_ti_3, Config_range_te_3

'''==============================================

Declare Sub inverse_problem_newton(h As Integer, z As Integer, step_te As Integer, step_ti As Integer)

Declare Sub ranges_set(h As Integer, delta_te As Integer, delta_ti As Integer)

Declare Sub results_write(h As Integer, num As Integer)

Declare Sub save_all()

'''==============================================


SEANS_DIR_OUT = "./out/"

SetEnviron("fbgfx=GDI")
Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1

Cls

Color 11
Print "UPRISE version " + UPRISE_VERSION + " (Wave Edition)"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Estimate - программа оценки параметров ионосферы (решение обратной задачи рассеяния) "
Print "(c) Богомаз А.В., Котов Д.В., Панасенко С.В. (Институт ионосферы)"
Print
Print "================================"
Print "Программа собрана " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
Print "================================"




file = FreeFile()
Open "config/config_wave.dat" For Input As #file
If Err <> 0 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

' 1
Input #file, Hmin

' 2
Input #file, Hmax

' 3
Input #file, Hstep

' 4
Input #file, LIBRARY_PATH

' 5
Input #file, Config_step_ti_1, Config_step_ti_2, Config_step_ti_3

' 6
Input #file, Config_step_te_1, Config_step_te_2, Config_step_te_3

' 7
Input #file, Config_range_ti_2, Config_range_ti_3

' 8
Input #file, Config_range_te_2, Config_range_te_3

Close #file





library_file = fopen (LIBRARY_PATH +"O+.lib", "rb")
If library_file = NULL Then
	PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
	End
EndIf





' загружаем массив температур (для которых рассчитаны библиотеки)
temperatures_len = library_oxygen_list_of_temperatures_get(@temperatures(0))


Color 11

Print "Результаты обработки, находящиеся в папке " + Chr(34) + "out" + Chr(34) + ":"
Color 10
Dim As String fn
fn = Dir("./out/*", fbDirectory)
While Len(fn) > 0
	fn = Dir()
	If (Len(fn)=13) And (Mid(fn, 7, 1)="-") Then
		Print fn;"   ";
	EndIf
Wend
Print
Print


Color 15
Print
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

SEANS_DIR_OUT = "./out/"
DirectoryOutput = ""

For i As Integer = 0 To d_ndays-1
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

seans_num = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"





DeleteDir(SEANS_DIR_OUT + DirectoryOutput+"/step3", FOF_NOCONFIRMATION Or FOF_SILENT)
MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")





file = FreeFile()
Open "filter.dat" For Input As #file
If Err <> 0 Then
	PrintErrorToLog(ErrorFilter, __FILE__, __LINE__)
	End
EndIf
For tau = 0 To num_point_acf-1
	Input #file, acf_filter(tau)
Next tau
Close #file






If as_file_load(SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+".0001", @as_file_in) = 0 Then ' загружаем файл (при этом выделяется память, поэтому её необходимо после освобождать)
	PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
EndIf

For h = 0 To NUM_H-1
	Hkm(h) = as_file_in.acf[h].h
Next h

as_file_close( @as_file_in ) ' освобождаем память, выделенную на сеанс





' создаём файл для значений высот
file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Output As #file
Close #file





' создаём файл для значений времени
file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Output As #file
Close #file





' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As dat_all_struct dat_all_str(0 To NUM_H-1, 0 To seans_num-1)
If Err() <> 0 Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf
Print "OK"




' Загружаем АКФ в ОЗУ
Print "Загрузка данных... ";
For t = 0 To seans_num-1 ' по времени
	Print_process_percent((t*100)/seans_num)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext

	If as_file_load(filename, @as_file_in) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
		End
	EndIf

	Dim As Integer hh, mm, ss
	Dim As Double time_decimal

	time_from_str(@hh, @mm, @ss, @as_file_in.time_ )
	time_decimal = time_2decimal(hh, mm, ss)

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Append As #file
	Print #file, Using "####.####"; time_decimal
	Close #file


	For h = 0 To NUM_H-1

		For tau = 0 To 18
			dat_all_str(h, t).acf(tau) = as_file_in.acf[h].rc(tau)
		Next tau

		dat_all_str(h, t).q = as_file_in.acf[h].q

	Next h

	as_file_close(@as_file_in)

Next t
Print_process_percent(1000)
Print "OK"
Print #1, Str(seans_num)+" files loaded"
Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"


Print
Print "Решение обратной задачи... "
z = 0
For h = Hmin To Hmax Step Hstep ' по высоте

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Q."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num-1 ' по времени
		Print #file, dat_all_str(h, t).q
	Next t
	Close #file



	Dim As Double qMin = 1e200
	For t = 0 To seans_num-1 ' по времени
		if dat_all_str(h, t).q < qMin Then
			qMin = dat_all_str(h, t).q
		EndIf
	Next t

	Dim As Double qMax = -1e200
	For t = 0 To seans_num-1 ' по времени
		if dat_all_str(h, t).q > qMax Then
			qMax = dat_all_str(h, t).q
		EndIf
	Next t


	Print Using "n = ###   h = ####   Qmin = ####.##   Qmax = ####.##"; h; Hkm(h); qMin; qMax

	' очистка погрешностей
	For t = 0 To seans_num-1
		dat_all_str(h, t).d_c = 1e200
	Next t

	' установка начальной точки АКФ
	For t = 0 To seans_num-1
		dat_all_str(h, t).nTau = 1
	Next t


	' 1 шаг
	inverse_problem_newton(h, z, Config_step_te_1, Config_step_ti_1)


	results_write(h, 1)


	' запись значения обработанной высоты в файл
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Append As #file
	Print #file, Str(CInt(Hkm(h)))
	Close #file

	z += 1
Next h

Print
Print "Сохранение результатов на диск... ";
Sleep(300)
save_all()
Print "OK"


Color 15
Print
Print "OK"
break



'''==============================================

Function calc_function(ti As Double, te As Double, acf() As Double) As Double

	Dim As Double acf_teor(0 To 255)
	Dim As Double d

	acf_3_full(1, 4, 16, 0, 0, ti, te, 1, 0, @acf_teor(0), 18, 30.555e-6)

	For tau As Integer = 1 To 18
		acf_teor(tau) *= 1-tau*delta_tau/pulse_length
	Next tau

	func_conv_d(@acf_teor(0), @acf_filter(0), @acf_teor(0), num_point_acf)
	array_norm0_d(@acf_teor(0), @acf_teor(0), num_point_acf)

	For tau As Integer = 1 To 18
		d += ( acf(tau) - acf(0)*acf_teor(tau) )^2
	Next tau
	Return d

End Function

Sub inverse_problem_newton(h As Integer, z As Integer, step_te As Integer, step_ti As Integer)

	Dim As Double dt = 10
	Dim As Double dt2 = dt*dt
	Dim As Double fTiTe, fTipTe, fTimTe, fTiTep, fTiTem, fTipTep
	Dim As Double hessian(0 To 1, 0 To 1)
	Dim As Double det
	Dim As Double hessian_1(0 To 1, 0 To 1)
	Dim As Double grad(0 To 1)
	Dim As Double temp(0 To 1)


	For t As Integer = 0 To seans_num-1 ' по времени


		Dim As Double ti, te

		If z = 0 Then
			ti = 500
			te = 500
		Else
			ti = dat_all_str(h - hStep, t).ti_c
			te = dat_all_str(h - hStep, t).te_c
		EndIf

		dat_all_str(h, t).ti_c = ti
		dat_all_str(h, t).te_c = te

		Dim As Integer n = 0

		Do

			fTiTe = calc_function(ti, te, dat_all_str(h, t).acf())
			fTipTe = calc_function(ti + dt, te, dat_all_str(h, t).acf())
			fTimTe = calc_function(ti - dt, te, dat_all_str(h, t).acf())
			fTiTep = calc_function(ti, te + dt, dat_all_str(h, t).acf())
			fTiTem = calc_function(ti, te - dt, dat_all_str(h, t).acf())
			fTipTep = calc_function(ti + dt, te + dt, dat_all_str(h, t).acf())

			hessian(0, 0) = (fTipTep - 2*fTiTe + fTimTe)/dt2
			hessian(0, 1) = (fTipTep - fTipTe - fTiTep + fTiTe)/dt2
			hessian(1, 0) = hessian(0, 1)
			hessian(1, 1) = (fTiTep - 2*fTiTe + fTiTem)/dt2

			det = hessian(0, 0)*hessian(1, 1) - hessian(0, 1)*hessian(1, 0)

			hessian_1(0, 0) = hessian(1, 1) / det
			hessian_1(1, 1) = hessian(0, 0) / det
			hessian_1(0, 1) = -hessian(0, 1) / det
			hessian_1(1, 0) = -hessian(1, 0) / det

			grad(0) = (fTipTe - fTimTe)/(2*dt)
			grad(1) = (fTiTep - fTiTem)/(2*dt)

			temp(0) = hessian_1(0, 0)*grad(0) + hessian_1(0, 1)*grad(1)
			temp(1) = hessian_1(1, 0)*grad(0) + hessian_1(1, 1)*grad(1)

			ti -= temp(0)
			te -= temp(1)

			If (te / ti >= 1 ) And (te <= 4000) And (te >= 500) And (ti <= 4000) And (ti >=500)  Then
				dat_all_str(h, t).ti_c = ti
				dat_all_str(h, t).te_c = te
			EndIf

			n += 1

		Loop Until n >= 50 Or Abs(temp(0)) < 3.5 And Abs(temp(1)) < 3.5

		Print #1, n
		'break

	Next t

End Sub


'''==============================================

Sub ranges_set(h As Integer, delta_te As Integer, delta_ti As Integer)
	For t As Integer = 0 To seans_num-1
		dat_all_str(h, t).te_start = dat_all_str(h, t).te_c-delta_te
		dat_all_str(h, t).te_end = dat_all_str(h, t).te_c+delta_te

		dat_all_str(h, t).ti_start = dat_all_str(h, t).ti_c-delta_ti
		dat_all_str(h, t).ti_end = dat_all_str(h, t).ti_c+delta_ti
	Next t
End Sub

'''==============================================

Sub results_write(h As Integer, num As Integer)

	Dim As Integer file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+Str(num)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t As Integer = 0 To seans_num-1
		Print #file, Using "####.# "; dat_all_str(h, t).ti_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+Str(num)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t As Integer = 0 To seans_num-1
		Print #file, Using "####.# "; dat_all_str(h, t).te_c
	Next t
	Close #file


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/D."+Str(num)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t As Integer = 0 To seans_num-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).d_c
	Next t
	Close #file


End Sub

'''==============================================

Sub save_all()

	Dim As Integer file

	Dim As Integer nH, nT
	Dim As Integer t, h

	Dim As Double temp

	' получаем количество сеансов
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/T.txt" For Input As #file

	nT = 0
	Do While Not Eof(file)
		Input #file, temp
		nT += 1
	Loop

	Close #file




	' получаем количество высот
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/H.txt" For Input As #file

	nH = 0
	Do While Not Eof(file)
		Input #file, temp
		nH += 1
	Loop

	Close #file



	' выделяем память
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)

	ReDim As Double  q_array  (0 To nT-1, 0 To nH-1)
	ReDim As Integer  n_array  (0 To nT-1, 0 To nH-1)

	ReDim As Double  ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double   d_array (0 To nT-1, 0 To nH-1)

	ReDim As Double  ti2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double   d2_array (0 To nT-1, 0 To nH-1)



	' считываем время сеансов
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/T.txt" For Input As #file

	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t

	Close #file



	' считываем значения высот
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/H.txt" For Input As #file


	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h

	Close #file




	' чтение данных из файла
	For h = 0 To nH-1

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Q."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, q_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/N."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, n_array(t, h)
		Next t
		Close #file


		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti.1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te.1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/D.1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, d_array(t, h)
		Next t
		Close #file


		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti.2."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti2_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te.2."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te2_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/D.2."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, d2_array(t, h)
		Next t
		Close #file

	Next h



	' запись Ti 1

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti1.txt" For Output As #file

	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; ti_array(t, h);
		Next h
	Next t

	Close #file


	' запись Ti 2

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti2.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; ti2_array(t, h);
		Next h
	Next t

	Close #file


	' запись Te 1

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te1.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; te_array(t, h);
		Next h
	Next t

	Close #file


	' запись Te 2

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te2.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; te2_array(t, h);
		Next h
	Next t

	Close #file




	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/N.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; n_array(t, h);
		Next h
	Next t

	Close #file



	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Q.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; q_array(t, h);
		Next h
	Next t

	Close #file



	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/D1.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "####      "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "##.##^^^^ "; d_array(t, h);
		Next h
	Next t

	Close #file




	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/D2.txt" For Output As #file


	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "####      "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "##.##^^^^ "; d2_array(t, h);
		Next h
	Next t

	Close #file


End Sub

