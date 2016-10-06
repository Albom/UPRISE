
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

Dim As as_file_struct	as_file_in
Dim As as_file_struct 	as_file_out

Dim As Integer is_divide

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename
Dim As Integer file

Dim Shared As Double qMaxDiurnalMin = 1e200
Dim Shared As Double qProfile(0 To 679)

Dim Shared As Double qLevel(0 To 255)
Dim Shared As Double qHint(0 To 255)
Dim Shared As Integer qNum = 0

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
Print "Программа поиска высот для разных уровней отношения сигнал/шум"
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


'выделяем память на профиль q с минимальным Qmax


file = FreeFile()
Open "config_trap.dat" For Input As #file

While (Not Eof(file))
	Input #file, qLevel(qNum)
	qNum += 1
Wend

Close #file

seans_num_out = seans_num_in


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


	Dim As Double qMax = -1e200
	For h = 0 To as_file_in.nh - 1
		If as_file_in.acf[h].q > qMax Then
			qMax = as_file_in.acf[h].q
		EndIf
	Next h

	If qMax < qMaxDiurnalMin Then
		qMaxDiurnalMin = qMax
		For h = 0 To as_file_in.nh - 1
			qProfile(h) = as_file_in.acf[h].q
		Next h
	EndIf




	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"


Dim As Integer nLevel = 0
For h = as_file_in.nh - 1 To 0 Step -1
	If nLevel >= qNum Then
		Exit For
	EndIf
	If qProfile(h) > qLevel(nLevel) Then
		qHint(nLevel) = h
		nLevel += 1
	EndIf
Next h

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput + "/step1/" + "config_trap.dat" For Output As #file
For h = 0 To qNum-1
	Print #file, Using "###.### #### ####.#"; qLevel(h); qHint(h); seans2_altL(qHint(h))
Next h

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput + "/step1/" + "profile.txt" For Output As #file
For h = 0 To 679
	Print #file, Using "####.# ###.###"; seans2_altL(h); qProfile(h)
Next h

Print
Print "OK"
break

'''==============================================
