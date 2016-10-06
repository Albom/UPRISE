
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
Dim As Integer f

Dim As as_file_struct	as_file_in
Dim As as_file_struct 	as_file_out

Dim As Integer is_divide

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename
Dim As Integer file, files(0 To 90)

Dim Shared As Double r_in(0 To 350)
Dim Shared As Double tau_in(0 To 350)

Dim Shared As Double r_out(0 To 350)
Dim Shared As Double tau_out(0 To 350)

Dim Shared As Double s_out(0 To 350)
Dim Shared As Double f_out(0 To 350)

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






Print "Выполнение преобразования Фурье... ";

seans_num_out = seans_num_in

MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step2")

For f = 0 To 90
	files(f) = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step2/" + Str(f) +".txt" For Output As #files(f)
Next f

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


	If t = 0 Then

		For f = 0 To 90
			Print #files(f), "  0     ";
			For h = 0 To 679
				Print #files(f), Using "#####.###   "; as_file_in.acf[h].h;
			Next h
			Print #files(f),
		Next f

	EndIf


	Dim As Integer hh, mm, ss

	For f = 0 To 90
		time_from_str(@hh, @mm, @ss, as_file_in.time_)
		Print #files(f), Using " ##.### "; time_2decimal(hh, mm, ss);
	Next f

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step2/S" + as_file_in.filename + ".txt" For Output As #file


	For h = 0 To 679

		For tau = 0 To 18

			r_in(tau) = as_file_in.acf[h].rc(tau)
			tau_in(tau) = tau * 30.555

		Next tau

		For tau = 0 To 180

			tau_out(tau) = tau * 3.0555
			r_out(tau) = array_linear_d(tau_out(tau), @tau_in(0), @r_in(0), 18)

		Next tau


		For f = 0 To 90
			f_out(f) = f * 100 '* (1.0 / 180)
			s_out(f) = 0
			For tau = 0 To 180
				s_out(f) += r_out(tau) * Cos(2 * M_PI * f_out(f) * tau * 3.0555e-6)
			Next tau
		Next f

		s_out(f) /= 90

		Print #file, Using "###  ####.### "; h; as_file_in.acf[h].h;

		For f = 0 To 90

			Print #file, Using "###.######^^^^ "; s_out(f);

		Next f

		Print #file,

		For f = 0 To 90
			Print #files(f), Using "###.###^^^^ "; s_out(f);
		Next f

	Next h

	Close #file

	For f = 0 To 90
		Print #files(f),
	Next f

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"


For f = 0 To 90
	Close #files(f)
Next f

Print
Print "OK"
break

'''==============================================
