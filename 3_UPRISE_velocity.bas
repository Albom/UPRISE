
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

Const LAMBDA = 1.8987
Const DELTA_TAU = 30.555e-6

'''==============================================

Dim As Integer file

Dim As Integer num_points
Dim As Integer nh
Dim As Integer Hmin, Hmax, Hstep

Dim As Integer i, j

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_OUT = "./out/"
Dim As String filename
Dim As String ext

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer seans_num_in


Dim As Integer t, h, tau

Dim As Double r2(0 To 100)


Dim As as_file_struct	as_file_in

Dim As Integer num_algo

'''==============================================

Hmin = 55
Hmax = 145
Hstep = 4

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Cls

Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Velocity - программа расчёта скорости движения плазмы"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15


file = FreeFile()
Open "config.dat" For Input As #file
If Err() <> 0 Then
	Input "Hmin: ", Hmin
	Input "Hmax: ", Hmax
	Input "Hstep: ", Hstep
Else
	Dim As String tmp

	Input #file, tmp
	Input #file, tmp
	Input #file, tmp
	Input #file, tmp
	Input #file, tmp

	Input #file, Hmin
	Input #file, Hmax
	Input #file, Hstep
EndIf

If (Hmin < 0) Or (Hmax < Hmax) Or (Hstep < 1)  Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf


Print
Color 12
Print "Проверьте параметры программы. При необходимости внесите исправления в config.dat и перезапустите программу."
Print
Color 15
Print "Номер минимальной высоты Hmin: "; Hmin
Print "Номер максимальной высоты Hmax: "; Hmax
Print "Шаг по высоте Hstep: "; Hstep
Print
Color 12
Print "Нажмите Enter"

Dim As Integer key
Do
	key = GetKey()
Loop Until key = KEY_ENTER

Print
Color 15


Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

Print
Input "Введите количество точек (от 1 до 18): ", num_points

If (num_points < 1) Or (num_points > 18) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

Print
Print "Учитывать вес каждой точки? (y/n) ";
num_algo = GetKey_YN()



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







If as_file_load(SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+".0001", @as_file_in) = 0 Then ' загружаем файл (при этом выделяется память, поэтому её необходимо после освобождать)
	PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
EndIf

nh = as_file_in.nh ' получаем число высот

ReDim Shared As Integer Hkm(0 To nh-1)

For h = 0 To nh-1
	Hkm(h) = as_file_in.acf[h].h
Next h

as_file_close( @as_file_in ) ' освобождаем память, выделенную на сеанс







Print
Print "Подсчёт количества входных файлов... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"








MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step4")







file = FreeFile
Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"T.txt" For Output As #file
Close #file







' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As Double drift_d (0 To nh-1, 0 To seans_num_in-1)
ReDim Shared As Double time_d (0 To seans_num_in-1)
Print "OK"








Print "Расчёт скорости движения плазмы... ";


For t = 0 To seans_num_in-1 ' по времени

	Print_process_percent((t*100)/seans_num_in)

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

	time_d(t) = time_decimal

	file = FreeFile
	Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"T.txt" For Append As #file
	Print #file, Using "###.####"; time_decimal
	Close #file


	For h = Hmin To Hmax Step Hstep ' по высоте

		If num_algo = 1 Then

			For tau = 1 To num_points
				r2(tau) = (as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0))^2 + (as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(0))^2
			Next tau

			Dim As Double c, z ' числитель и знаменатель

			c = 0
			For tau = 1 To num_points
				If as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0) > 0.05 Then
					c += Atn( as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) ) * r2(tau) * DELTA_TAU * tau
				EndIf
			Next tau

			z = 0
			For tau = 1 To num_points
				If as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0) > 0.05 Then
					z += r2(tau) * (DELTA_TAU * tau)^2
				EndIf
			Next tau

			drift_d(h, t) = -LAMBDA/(4*M_PI)*c/z

		Else

			Dim As Double c ' количество слагаемых
			c = 0
			drift_d(h, t) = 0
			For tau = 1 To num_points
				If as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0) > 0.05 Then
					drift_d(h, t) += Atn( as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) ) / (DELTA_TAU * tau)
					c += 1
				EndIf
			Next tau

			If c <> 0 Then
				drift_d(h, t) *= -LAMBDA/(4*M_PI)/c
			Else
				drift_d(h, t) = 0
			EndIf

		EndIf

	Next h

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"








' Вывод результатов в файлы
Print "Вывод результатов в файлы... ";

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"H.txt" For Output As #file
For h = Hmin To Hmax Step Hstep ' по высоте
	Print #file, Str(CInt(Hkm(h)))
Next h
Close #file

For h = Hmin To Hmax Step Hstep ' по высоте

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step4/V."+ Str(CInt(Hkm(h))) +".txt" For Output As #file

	For t = 0 To seans_num_in-1
		Print #file, Using "#####.###  "; drift_d(h, t)
	Next t

	Close #file

Next h



file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"V.txt"  For Output As #file
If Err() <> 0 Then
	break
EndIf

Print #file, "      0 ";

For h = Hmin To Hmax Step Hstep
	Print #file, Using "########  "; CInt(Hkm(h));
Next h

For t As Integer = 0 To seans_num_in-1
	Print #file,
	Print #file, Using "##.#### "; time_d(t);
	For h = Hmin To Hmax Step Hstep
		Print #file, Using "#####.##  "; drift_d(h, t);
	Next h
Next t

Close #file



Print "OK"










Print
Print "OK"
break

'''==============================================
