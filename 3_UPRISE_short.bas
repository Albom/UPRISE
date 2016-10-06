
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "fbgfx.bi"			'Подключение графической библиотеки

#Include "windows.bi"
#Include "file.bi"

#Include "window9.bi"

'''==============================================

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf

'''==============================================

Dim As Integer i, j

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim Shared As String SEANS_DIR_OUT
Dim As String filename
Dim As String ext

Dim As String Directory
Dim Shared As String DirectoryOutput

Dim As Integer seans_num_in
Dim Shared As Integer seans_num_out

Dim As Integer t, tau, h, lag
Dim As Integer z


Dim As Integer nh'число высот


Dim As as_file_struct	as_file_in



'ReDim Shared As dat_all_struct dat_all_str(0 To 679, 0 To 99)
ReDim Shared As Integer Hkm(0 To 679)


Dim As Integer file



'''==============================================

SEANS_DIR_OUT = "./out/"


SetEnviron("fbgfx=GDI")
Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1


Cls

Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Short - программа сохранения данных, полученных с использованием короткого импульса"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15

Print
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays



SEANS_DIR_OUT = "./out/"
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




Print
Print "Подсчёт количества входных файлов... ";


seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"



seans_num_out = seans_num_in

MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")


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




' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As Double qShort(0 To nh-1, 0 To seans_num_out-1)
If Err() <> 0 Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

ReDim Shared As Double time_decimal(0 To seans_num_out-1)
If Err() <> 0 Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

Print "OK"



' Загружаем АКФ в ОЗУ
Print "Загрузка данных... ";
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

	Print #1, "AS"+DirectoryOutput+"."+ext

	Dim As Integer hh, mm, ss


	time_from_str(@hh, @mm, @ss, @as_file_in.time_ )
	time_decimal(t) = time_2decimal(hh, mm, ss)

	For h = 0 To nh-1
		qShort(h, t) = as_file_in.acf[h].qshort
	Next h

	as_file_close(@as_file_in)

Next t
Print_process_percent(1000)
Print "OK"

Print #1, Str(seans_num_out)+" files loaded"

Print "Запись данных... ";

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Qshort.txt" For Output As #file

Print #file, Using "######.#### "; 0;
For h = 0 To nh-1
	Print #file, Using "       #### "; Hkm(h);
Next h
Print #file,


For t = 0 To seans_num_out-1 ' по времени
	Print_process_percent((t*100)/seans_num_out)
	Print #file, Using "######.#### "; time_decimal(t);
	For h = 0 To nh-1
		Print #file, Using "######.#### "; qShort(h, t);
	Next h
	Print #file,
Next t

Close #file

Print_process_percent(1000)
Print "OK"

Print
Print "OK"
break
