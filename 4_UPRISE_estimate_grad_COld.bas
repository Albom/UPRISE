
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

Type dat_all_struct

	Dim	acf(0 To 18)	As Double
	Dim	p_corr			As Double
	Dim	q					As Double

	Dim	d_c				As Double
	Dim	ti_c				As Double	' температура ионов
	Dim	te_c				As Double	' температура электронов
	Dim	ox_c				As Double	' относительное содержание ионов кислорода

	Dim	ti_start			As Double
	Dim	te_start			As Double
	Dim	ox_start			As Double

	Dim	ti_end			As Double
	Dim	te_end			As Double
	Dim	ox_end			As Double

End Type

'''==============================================

Const SEANS_DIR_OUT = "./out/"
Dim Shared  As String LIBRARY_PATH
Dim As String Directory
Dim Shared As String DirectoryOutput

Dim Shared As Integer nT 'число сеансов
Dim Shared As Integer nH 'число высот
Dim Shared As Integer nTau = 27 '
Dim Shared As Integer hMin, hMax, hStep

Dim Shared As Integer libraries_num = 101
Dim Shared As Integer libraries_min = 101
Dim Shared As FILE Ptr libraries_file(0 To 100)

Dim Shared As Double  acf_filter(0 To 100) ' АКФ фильтра

Dim Shared As Integer h, t, tau

Dim Shared As Integer file

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As as_file_struct	as_file

Dim Shared As Integer temperatures(0 To 200000)
Dim Shared As Integer temperatures_len

Dim As String tmp_string

Dim As Integer ox

Dim Shared As Double   Config_coeff(0 To 18)

Dim Shared As Integer  Config_step_ox_1,  Config_step_ti_1,  Config_step_te_1
Dim Shared As Integer Config_range_ox_2, Config_range_ti_2, Config_range_te_2
Dim Shared As Integer  Config_step_ox_2,  Config_step_ti_2,  Config_step_te_2
Dim Shared As Integer Config_range_ox_3, Config_range_ti_3, Config_range_te_3
Dim Shared As Integer  Config_step_ox_3,  Config_step_ti_3,  Config_step_te_3


'''==============================================

Declare Sub results_write(ByVal h As Integer)
Declare Sub inverse_problem_v1(ByVal h As Integer, ByVal step_ox As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v2(ByVal h As Integer, ByVal step_ox As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub ranges_set(ByVal h As Integer, ByVal delta_ox As Integer, ByVal delta_te As Integer, ByVal delta_ti As Integer)

'''==============================================


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
Print "Estimate - программа оценки параметров ионосферы (решение обратной задачи рассеяния)"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15



file = FreeFile()
Open "config_COld.dat" For Input As #file

Input #file, tmp_string

Input #file, LIBRARY_PATH

Input #file, hMin
Input #file, hMax
Input #file, hStep

For tau = 0 To 18
	Input #file, Config_coeff(tau)
Next tau

Input #file, Config_step_ox_1,  Config_step_ti_1,  Config_step_te_1
Input #file, Config_range_ox_2, Config_range_ti_2, Config_range_te_2
Input #file, Config_step_ox_2,  Config_step_ti_2,  Config_step_te_2
Input #file, Config_range_ox_3, Config_range_ti_3, Config_range_te_3
Input #file, Config_step_ox_3,  Config_step_ti_3,  Config_step_te_3

Close #file


Print
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays




DirectoryOutput = ""

For t = 0 To d_ndays-1
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

		If t = 0 Then
			DirectoryOutput += directory + "-"
		EndIf
	EndIf
Next t
DirectoryOutput += directory



Print
Print "Подсчёт количества входных файлов... ";


nT = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If nT < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"


DeleteDir(SEANS_DIR_OUT + DirectoryOutput+"/step3", FOF_NOCONFIRMATION Or FOF_SILENT)
MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")


file = FreeFile()
Open "filter_COld.dat" For Input As #file
If Err <> 0 Then
	PrintErrorToLog(ErrorFilter, __FILE__, __LINE__)
	End
EndIf
For tau = 0 To nTau-1
	Input #file, acf_filter(tau)
Next tau
Close #file


If as_file_load(SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+".0001", @as_file) = 0 Then ' загружаем файл (при этом выделяется память, поэтому её необходимо после освобождать)
	PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
EndIf


nH = as_file.nH ' получаем число высот



ReDim Shared As Integer hKm(0 To nh-1)

For h = 0 To nH-1
	hKm(h) = as_file.acf[h].h
Next h

as_file_close( @as_file ) ' освобождаем память, выделенную на сеанс



' создаём файл для значений высот
file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Output As #file
Close #file


temperatures_len = library_heavy_list_of_temperatures_get(@temperatures(0))




' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As dat_all_struct dat_all_str(0 To nH-1, 0 To nT-1)
Print "OK"

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Output As #file
Close #file

' Загружаем АКФ в ОЗУ
Print "Загрузка данных... ";
For t = 0 To nT-1 ' по времени

	Dim As String ext, filename

	Print_process_percent((t*100)/nT)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext

	If as_file_load(filename, @as_file) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
		End
	EndIf

	Print #1, "AS"+DirectoryOutput+"."+ext

	Dim As Integer hh, mm, ss
	Dim As Double time_decimal

	time_from_str(@hh, @mm, @ss, @as_file.time_ )
	time_decimal = time_2decimal(hh, mm, ss)

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Append As #file
	Print #file, Using "####.####"; time_decimal
	Close #file


	For h = 0 To nH-1

		For tau = 0 To 18
			dat_all_str(h, t).acf(tau) = as_file.acf[h].rc(tau)
		Next tau

		dat_all_str(h, t).d_c = 1e200

		dat_all_str(h, t).q = as_file.acf[h].q

	Next h

	as_file_close(@as_file)

Next t
Print_process_percent(1000)
Print "OK"

Print #1, Str(nT)+" files loaded"



' Загружаем библиотеки АКФ
Print "Загрузка библиотек АКФ... ";
For ox = 0 To libraries_num-1

	Print_process_percent((ox*100)/100)

	Dim As ZString*256 fn

	fn = Str(ox)
	If ox < 10  Then fn = "0"+fn
	If ox < 100 Then fn = "0"+fn
	fn = LIBRARY_PATH + "O=" + fn + ".dat"

	libraries_file(ox) = fopen (fn, "rb")
	If libraries_file(ox) = NULL Then
		PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
		End
	EndIf

Next ox

Print_process_percent(1000)
Print "OK"


Dim Shared As Double RegRange(0 To 5, 0 To nT-1)
For t = 0 To nT-1 ' по времени
	RegRange(0, t) = 200
	RegRange(1, t) = 3500
	RegRange(2, t) = 20
	RegRange(3, t) = 3500
	RegRange(4, t) = 100
	RegRange(5, t) = 100
Next t

For h = hMax To hMin Step -hStep ' по высоте

	Cls
	Print "Решение обратной задачи... "
	Print #1, "Inverse problem solving... h="+Str(CInt(Hkm(h)))+" km"

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Q."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To nT-1 ' по времени
		Print #file, dat_all_str(h, t).q
	Next t
	Close #file


	' 1 шаг
	inverse_problem_v1(h, Config_step_ox_1, Config_step_te_1, Config_step_ti_1)

	ranges_set(h, Config_range_ox_2, Config_range_te_2, Config_range_ti_2)
	inverse_problem_v2(h, Config_step_ox_2, Config_step_te_2, Config_step_ti_2)

	ranges_set(h, Config_range_ox_3, Config_range_te_3, Config_range_ti_3)
	inverse_problem_v2(h, Config_step_ox_3, Config_step_te_3, Config_step_ti_3)


	results_write(h)

	' запись значения обработанной высоты в файл
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Append As #file
	Print #file, Str(CInt(Hkm(h)))
	Close #file

	libraries_min = 1
	For t = 0 To nT-1 ' по времени
		RegRange(0, t) = dat_all_str(h, t).te_c-100
		RegRange(1, t) = dat_all_str(h, t).te_c+10
		RegRange(2, t) = dat_all_str(h, t).ti_c-100
		RegRange(3, t) = dat_all_str(h, t).ti_c+10
		RegRange(4, t) = dat_all_str(h, t).ox_c-20
	Next t

Next h



break



Sub inverse_problem_v1(ByVal h As Integer, ByVal step_ox As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)

	Dim As Integer ox
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d

	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)


	For ox = libraries_num-1 To libraries_min-1 Step -step_ox

		For te = 200 To 4000 Step step_te

			For ti = 200 To 4000 Step step_ti

				If (te/ti <= 3.5) And (te/ti >= 0.5) Then

					If acf_library_heavy( libraries_file(ox), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), nTau) <> 0 Then

						func_conv_d(@acf_teor(0), @acf_filter(0), @acf_teor(0), nTau)
						array_norm0_d(@acf_teor(0), @acf_teor(0), nTau)

						For t = 0 To nT-1 ' по времени

							If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) And ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) )  Then

								d = 0
								For tau = 1 To 18
									d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
								Next tau

								If d < dat_all_str(h, t).d_c Then
									dat_all_str(h, t).d_c = d
									dat_all_str(h, t).ti_c = ti
									dat_all_str(h, t).te_c = te
									dat_all_str(h, t).ox_c = ox
								EndIf


							EndIf

						Next t

					EndIf

				EndIf

			Next ti

		Next te

	Next ox


End Sub

''' ================================================================

Sub inverse_problem_v2(ByVal h As Integer, ByVal step_ox As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)

	Dim As Integer ox
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d

	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)


	For t = 0 To nT-1 ' по времени

		For ox = dat_all_str(h, t).ox_start To dat_all_str(h, t).ox_end Step step_ox

			If ( ox >= RegRange(4, t) ) And ( ox <= RegRange(5, t) ) Then

				For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te

					If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) Then

						For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti

							If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) Then

								If (te/ti <= 3.5) And (te/ti >= 0.5) Then

									If acf_library_heavy( libraries_file(ox), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), nTau) <> 0 Then

										func_conv_d(@acf_teor(0), @acf_filter(0), @acf_teor(0), nTau)
										array_norm0_d(@acf_teor(0), @acf_teor(0), nTau)

										d = 0
										For tau = 1 To 18
											d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
										Next tau

										If d < dat_all_str(h, t).d_c Then
											dat_all_str(h, t).d_c = d
											dat_all_str(h, t).ti_c = ti
											dat_all_str(h, t).te_c = te
											dat_all_str(h, t).ox_c = ox
										EndIf

									EndIf

								EndIf

							EndIf

						Next ti

					EndIf

				Next te

			EndIf

		Next ox

	Next t



End Sub


''' ================================================================


Sub results_write(ByVal h As Integer)

	Dim As Integer file
	Dim As Integer t

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "O."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To nT-1
		Print #file, Using "###.# "; dat_all_str(h, t).ox_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To nT-1
		Print #file, Using "####.# "; dat_all_str(h, t).ti_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To nT-1
		Print #file, Using "####.# "; dat_all_str(h, t).te_c
	Next t
	Close #file


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To nT-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).d_c
	Next t
	Close #file

End Sub


''' ================================================================


Sub ranges_set(ByVal h As Integer, ByVal delta_ox As Integer, ByVal delta_te As Integer, ByVal delta_ti As Integer)

	Dim As Integer t

	For t = 0 To nT-1
		dat_all_str(h, t).te_start = dat_all_str(h, t).te_c-delta_te
		dat_all_str(h, t).te_end = dat_all_str(h, t).te_c+delta_te

		dat_all_str(h, t).ti_start = dat_all_str(h, t).ti_c-delta_ti
		dat_all_str(h, t).ti_end = dat_all_str(h, t).ti_c+delta_ti

		dat_all_str(h, t).ox_start = dat_all_str(h, t).ox_c-delta_ox
		dat_all_str(h, t).ox_end = dat_all_str(h, t).ox_c+delta_ox

		If dat_all_str(h, t).ox_start > 100 Then
			dat_all_str(h, t).ox_start = 100
		EndIf

		If dat_all_str(h, t).ox_end > 100 Then
			dat_all_str(h, t).ox_end = 100
		EndIf


		If dat_all_str(h, t).ox_start < 1 Then
			dat_all_str(h, t).ox_start = 1
		EndIf

		If dat_all_str(h, t).ox_end < 1 Then
			dat_all_str(h, t).ox_end = 1
		EndIf

	Next t

End Sub

