
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами

#Include "crt/stdlib.bi"
#Include "fbgfx.bi"			'Подключение графической библиотеки

#Include "windows.bi"
#Include "file.bi"

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
	Dim	hyd_c				As Double	' относительное содержание ионов водорода
	Dim	he_c				As Double	' относительное содержание ионов гелия

	Dim	ti_start			As Double
	Dim	te_start			As Double
	Dim	hyd_start		As Double

	Dim	ti_end			As Double
	Dim	te_end			As Double
	Dim	hyd_end			As Double

End Type

'''==============================================

Const As Double delta_tau = 30.555

'''==============================================

Dim Shared  As String LIBRARY_PATH
Dim Shared  As Double pulse_length '663.0 ' или 795.0
Dim Shared  As Integer isConv
Dim Shared	As Integer num_point_acf ' 22 или 26, вычисляется автоматически (pulse_length/delta_tau)

Dim Shared As Double DeltaTePlus   ' максимальный градиент температуры электронов
Dim Shared As Double DeltaTeMinus  ' максимальный градиент температуры электронов
Dim Shared As Double DeltaTiPlus   ' максимальный градиент температуры ионов
Dim Shared As Double DeltaTiMinus  ' максимальный градиент температуры ионов
Dim Shared As Double DeltaHydPlus  ' максимальный градиент Hyd (60 - это 30%)
Dim Shared As Double DeltaHydMinus ' максимальный градиент Hyd (60 - это 30%)

Dim Shared As Double  RegN
Dim Shared As Integer RegWnd			' окно для расчёта СКО

Dim Shared As Integer  Config_step_h_1,  Config_step_ti_1,  Config_step_te_1
Dim Shared As Integer Config_range_h_2, Config_range_ti_2, Config_range_te_2
Dim Shared As Integer  Config_step_h_2,  Config_step_ti_2,  Config_step_te_2
Dim Shared As Integer Config_range_h_3, Config_range_ti_3, Config_range_te_3
Dim Shared As Integer  Config_step_h_3,  Config_step_ti_3,  Config_step_te_3
Dim Shared As Integer Config_range_h_4, Config_range_ti_4, Config_range_te_4
Dim Shared As Integer  Config_step_h_4,  Config_step_ti_4,  Config_step_te_4
Dim Shared As Integer  Config_oxygen

Dim Shared As Integer he_step

Dim Shared As Integer Config_sinus

'''==============================================

Dim Shared As Integer START_X = 0
Dim Shared As Integer CUR = 0

Dim Shared As Integer He_max
Dim Shared As Integer He_max2
Dim Shared As Integer He_grad
Dim Shared As Integer He_maxLib

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

Dim Shared As Integer Hmin, Hmax, Hstep

Dim As as_file_struct	as_file_in



ReDim Shared As dat_all_struct dat_all_str(0 To 679, 0 To 99)
ReDim Shared As Integer Hkm(0 To 679)



Dim Shared As Integer temperatures(0 To 200000)
Dim Shared As Integer temperatures_len

Dim As Integer file

Dim As Double ti, te
Dim As Integer hyd

Dim Shared As ZString*256 zfilename

Dim Shared libraries_filelist As ZString*200000

Dim As Double d

Dim Shared As Integer libraries_num = 0
Dim Shared As FILE Ptr libraries_file(0 To 300)


ReDim Shared As Double RegRange(0 To 5, 0 To 1500) ' диапазон для регуляризации

Dim As Integer He
Dim Shared As Integer heCurrent

Dim Shared As Double  acf_filter(0 To 100) ' АКФ фильтра


Dim Shared As Double coeffTau(0 To 18) ' коэффициенты для задержек


Enum Params
PARAM_TI
PARAM_TE
PARAM_H
PARAM_HE
End Enum

'''==============================================

Declare Sub inverse_problem_v1_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v2_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)

Declare Sub inverse_problem_he(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)
Declare Sub inverse_problem_ti(ByVal h As Integer, ByVal z As Integer)
Declare Sub inverse_problem_te(ByVal h As Integer, ByVal z As Integer)
Declare Sub inverse_problem_hyd(ByVal h As Integer, ByVal z As Integer)
Declare Sub inverse_problem_ti_te(ByVal h As Integer, ByVal z As Integer)
Declare Sub inverse_problem_hyd_ti_te(ByVal h As Integer, ByVal z As Integer)

Declare Sub ranges_set(ByVal h As Integer, ByVal delta_hyd As Integer, ByVal delta_te As Integer, ByVal delta_ti As Integer)
Declare Sub ranges_reset(ByVal h As Integer, t_start As Integer = -1 , t_end As Integer = -1)
Declare Sub ranges_reg_set(ByVal h As Integer)

Declare Sub results_write(ByVal h As Integer, ByVal He As Integer)
Declare Sub intervals_input(ByVal h As Integer)
Declare Sub intervals_input_auto(ByVal h As Integer)
Declare Sub input_param(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, ByVal param As Integer)

Declare Sub draw_all(ByVal h As Integer, ByVal z As Integer)
Declare Sub draw_d(ByVal h As Integer, ByVal z As Integer)
Declare Sub draw_help()

Declare Sub interpolate_param(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, ByVal param As Integer)

Declare Sub undo(ByVal h As Integer)
Declare Sub save(ByVal h As Integer)

Declare Sub param_load(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)
Declare Sub param_save(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)
Declare Sub param_load2(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)

Declare Sub trand_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)
Declare Sub trand_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)
Declare Sub trand_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)

Declare Sub level_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)
Declare Sub level_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)
Declare Sub level_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)

Declare Sub equate_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)

Declare Sub poly_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)
Declare Sub poly_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)
Declare Sub poly_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)

Declare Sub spline_wnd_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer, degree As Integer)
Declare Sub spline_wnd_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer, degree As Integer)

Declare Sub gradient_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)

Declare Function getHeMax(h As Integer) As Integer


'''==============================================

SEANS_DIR_OUT = "./out/"


SetEnviron("fbgfx=GDI")
Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1


For tau = 0 To 18
	coeffTau(tau) =  ( tau / ( 1 - tau/22.0 ) ) ^2
Next tau


Cls

Color 11
Print "UPRISE version 1.1 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Estimate - программа оценки параметров ионосферы (решение обратной задачи рассеяния)"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15


file = FreeFile()
Open "config.dat" For Input As #file
If Err() > 0 Then
	Input "Введите длительность зондирующего импульса (мкс): ", pulse_length
	LIBRARY_PATH = "d:/lib/"
	isConv = 0

	Input "Hmin: ", Hmin
	Input "Hmax: ", Hmax
	Input "Hstep: ", Hstep

	Input "He max на первой высоте: ", He_max
	Input "He max на других высотах: ", He_max2

	Input "|+deltaTe|: ",   DeltaTePlus   ' максимальный градиент температуры электронов
	Input "|-deltaTe|: ",   DeltaTeMinus  ' максимальный градиент температуры электронов
	Input "|+deltaTi|: ",   DeltaTiPlus   ' максимальный градиент температуры ионов
	Input "|-deltaTi|: ",   DeltaTiMinus  ' максимальный градиент температуры ионов
	Input "|+deltaHyd|: ",  DeltaHydPlus  ' максимальный градиент Hyd (60 - это 30%)
	Input "|-deltaHyd|: ",  DeltaHydMinus
	Input "N СКО: ",   RegN
	Input "Ширина окна для расчёта СКО: ",   RegWnd

	Config_step_h_1  = 10 : Config_step_ti_1  = 100 :  Config_step_te_1 = 100
	Config_range_h_2 = 20 : Config_range_ti_2 = 200 : Config_range_te_2 = 200
	Config_step_h_2  = 5  :  Config_step_ti_2 = 50  :  Config_step_te_2 = 50
	Config_range_h_3 = 10 : Config_range_ti_3 = 100 : Config_range_te_3 = 100
	Config_step_h_3  = 2  : Config_step_ti_3  = 20  : Config_step_te_3  = 20
	Config_range_h_4 = 4  : Config_range_ti_4 = 40  : Config_range_te_4 = 40
	Config_step_h_4  = 1  : Config_step_ti_4  = 10  : Config_step_te_4  = 10

	Config_oxygen = 0

	he_step = 1

	Config_sinus = 1

Else

	Dim As String tmp_string

	Input #file, tmp_string ' в первой строке записано имя файла характеристики разрядника, в данной программе она не используется

	Input #file, isConv
	Input #file, LIBRARY_PATH
	Input #file, pulse_length

	Input #file, Hmin
	Input #file, Hmax
	Input #file, Hstep

	Input #file, He_max
	Input #file, He_max2

	Input #file, DeltaTePlus   ' максимальный градиент температуры электронов
	Input #file, DeltaTeMinus  ' максимальный градиент температуры электронов
	Input #file, DeltaTiPlus   ' максимальный градиент температуры ионов
	Input #file, DeltaTiMinus  ' максимальный градиент температуры ионов
	Input #file, DeltaHydPlus  ' максимальный градиент Hyd (60 - это 30%)
	Input #file, DeltaHydMinus

	Input #file, RegN
	Input #file, RegWnd

	Input #file, Config_step_h_1,  Config_step_ti_1,  Config_step_te_1
	Input #file, Config_range_h_2, Config_range_ti_2, Config_range_te_2
	Input #file, Config_step_h_2,  Config_step_ti_2,  Config_step_te_2
	Input #file, Config_range_h_3, Config_range_ti_3, Config_range_te_3
	Input #file, Config_step_h_3,  Config_step_ti_3,  Config_step_te_3
	Input #file, Config_range_h_4, Config_range_ti_4, Config_range_te_4
	Input #file, Config_step_h_4,  Config_step_ti_4,  Config_step_te_4

	Input #file, he_step
	Input #file, Config_oxygen

	Input #file, Config_sinus

	Input #file, He_grad
	Input #file, He_maxLib

	Close #file

EndIf

DeltaTePlus   = Abs(DeltaTePlus)
DeltaTeMinus  = Abs(DeltaTeMinus)
DeltaTiPlus   = Abs(DeltaTiPlus)
DeltaTiMinus  = Abs(DeltaTiMinus)
DeltaHydPlus  = Abs(DeltaHydPlus)
DeltaHydMinus = Abs(DeltaHydMinus)

Print
Color 12
Print "Проверьте параметры программы. При необходимости внесите исправления в config.dat и перезапустите программу."
Print
Color 15
Print "Длительность зондирующего импульса: "; pulse_length; " мкс"
Print "Путь к библиотекам АКФ: "; LIBRARY_PATH
Print
Print "Номер минимальной высоты Hmin: "; Hmin
Print "Номер максимальной высоты Hmax: "; Hmax
Print "Шаг по высоте Hstep: "; Hstep

If Config_oxygen = 0 Then
	Print
	Print "He max на первой высоте: ";  He_max; " %"
	Print "He max на других высотах: "; He_max2; " %"
EndIf

Print
Print "Градиенты по Te (на 4.5 км): +"; DeltaTePlus; " K, -"; DeltaTeMinus; " K"
Print "Градиенты по Ti (на 4.5 км): +"; DeltaTiPlus; " K, -"; DeltaTiMinus; " K"

If Config_oxygen = 0 Then
	Print "Градиенты по H+ (на 4.5 км): +"; DeltaHydPlus; ", -"; DeltaHydMinus, "("; DeltaHydPlus/2; " %, -"; DeltaHydMinus/2; " % )"
EndIf


Print "Количество СКО: "; RegN
Print "Ширина окна для расчёта СКО: "; RegWnd

Print

If Config_oxygen = 0 Then

	Print "Шаги при 1-м приближении (H+, Ti, Te):      ";  Config_step_h_1; " ("; Config_step_h_1/2;" % ), " ; Config_step_ti_1; " K, "; Config_step_te_1; " K"
	Print
	Print "Интервалы при 2-м приближении (H+, Ti, Te):   +-";  Config_range_h_2; " (+-"; Config_range_h_2/2;" % ), +-" ; Config_range_ti_2; " K, +-"; Config_range_te_2; " K"
	Print "Шаги при 2-м приближении (H+, Ti, Te):      ";  Config_step_h_2; " ("; Config_step_h_2/2;" % ), " ; Config_step_ti_2; " K, "; Config_step_te_2; " K"
	Print
	Print "Интервалы при 3-м приближении (H+, Ti, Te):   +-";  Config_range_h_3; " (+-"; Config_range_h_3/2;" % ), +-" ; Config_range_ti_3; " K, +-"; Config_range_te_3; " K"
	Print "Шаги при 3-м приближении (H+, Ti, Te):      ";  Config_step_h_3; " ("; Config_step_h_3/2;" % ), " ; Config_step_ti_3; " K, "; Config_step_te_3; " K"
	Print
	Print "Интервалы при 4-м приближении (H+, Ti, Te):   +-";  Config_range_h_4; " (+-"; Config_range_h_4/2;" % ), +-" ; Config_range_ti_4; " K, +-"; Config_range_te_4; " K"
	Print "Шаги при 4-м приближении (H+, Ti, Te):      ";  Config_step_h_4; " ("; Config_step_h_4/2;" % ), " ; Config_step_ti_4; " K, "; Config_step_te_4; " K"

	Print
	Print "Шаг по He+: ", he_step; "%"
	Print "Градиент по He+ (на 4.5 км): "; he_grad; "%"
Else

	Print "Шаги при 1-м приближении (Ti, Te):      ";   Config_step_ti_1; " K, "; Config_step_te_1; " K"
	Print
	Print "Интервалы при 2-м приближении (Ti, Te):   +-";  Config_range_ti_2; " K, +-"; Config_range_te_2; " K"
	Print "Шаги при 2-м приближении (Ti, Te):      ";  Config_step_ti_2; " K, "; Config_step_te_2; " K"
	Print
	Print "Интервалы при 3-м приближении (Ti, Te):   +-";  Config_range_ti_3; " K, +-"; Config_range_te_3; " K"
	Print "Шаги при 3-м приближении (Ti, Te):      "; Config_step_ti_3; " K, "; Config_step_te_3; " K"
	Print
	Print "Интервалы при 4-м приближении (Ti, Te):   +-";  Config_range_ti_4; " K, +-"; Config_range_te_4; " K"
	Print "Шаги при 4-м приближении (Ti, Te):      ";  Config_step_ti_4; " K, "; Config_step_te_4; " K"

EndIf

If Config_oxygen <> 0 Then
	Color 12
	Print
	Print "Внимение! Используется кислородное приближение."
EndIf

Color 15
Print
If Config_sinus <> 0 Then
	Print "Синусная составляющая учитывается."
Else
	Print "Синусная составляющая не учитывается."
EndIf


Print
Color 12
Print "Нажмите Enter"

Dim As Integer key
Do
	key = GetKey()
Loop Until key = KEY_ENTER



DeltaTePlus   *= Hstep
DeltaTeMinus  *= Hstep
DeltaTiPlus   *= Hstep
DeltaTiMinus  *= Hstep
DeltaHydPlus  *= Hstep
DeltaHydMinus *= Hstep

He_grad *= Hstep


Cls()
Color 15, 0


num_point_acf = pulse_length/delta_tau

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

FileCopy("config.dat", SEANS_DIR_OUT + DirectoryOutput+"/step3/config.dat")

If isConv = 0 Then

	' загрузка АКФ ИХ фильтра
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

EndIf




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




' создаём файл для значений высот
file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Output As #file
Close #file






temperatures_len = fortran_library_list_of_temperatures_get_short_663(@temperatures(0)) ' загружаем массив температур (для которых рассчитаны библиотеки)





' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As dat_all_struct dat_all_str(0 To nh-1, 0 To seans_num_out-1)
ReDim Shared As Double noise_acf(0 To seans_num_out-1, 0 To 255)

ReDim Shared As Double heRange(0 To seans_num_out-1)
For t = 0 To seans_num_out-1
	heRange(t) = He_max
Next t



ReDim Shared As Double RegRange(0 To 5, 0 To seans_num_out-1)
For t = 0 To seans_num_out-1
	RegRange(0, t) = 0
	RegRange(1, t) = 10000
	RegRange(2, t) = 0
	RegRange(3, t) = 10000
	RegRange(4, t) = -1
	RegRange(5, t) = 300
Next t

Print "OK"






file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Output As #file
Close #file

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
	Dim As Double time_decimal

	time_from_str(@hh, @mm, @ss, @as_file_in.time_ )
	time_decimal = time_2decimal(hh, mm, ss)

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"T.txt" For Append As #file
	Print #file, Using "####.####"; time_decimal
	Close #file


	For h = 0 To nh-1

		If Config_sinus <> 0 Then
			For tau = 0 To 18
				dat_all_str(h, t).acf(tau) = Sqr(as_file_in.acf[h].rc(tau)^2 + as_file_in.acf[h].rs(tau)^2)*Sgn( as_file_in.acf[h].rc(tau) )
			Next tau
		Else
			For tau = 0 To 18
				dat_all_str(h, t).acf(tau) = as_file_in.acf[h].rc(tau)
			Next tau
		EndIf

		dat_all_str(h, t).p_corr = as_file_in.acf[h].pcorr' скорректированный профиль мощности

		dat_all_str(h, t).d_c = 1e200

		dat_all_str(h, t).q = as_file_in.acf[h].q

	Next h

	For tau = 0 To 18
		noise_acf(t, tau) = as_file_in.rnc(tau)
	Next tau


	as_file_close(@as_file_in)

Next t
Print_process_percent(1000)
Print "OK"

Print #1, Str(seans_num_out)+" files loaded"













' Решение обратной задачи

If Config_oxygen <> 0 Then
	he_max = 0
EndIf


z = 0
For h = Hmin To Hmax Step Hstep ' по высоте

	Cls
	Print "Решение обратной задачи... "
	Print #1, "Inverse problem solving... h="+Str(CInt(Hkm(h)))+" km"




	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Q."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1 ' по времени
		Print #file, dat_all_str(h, t).q
	Next t
	Close #file



	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "P."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1 ' по времени
		Print #file, dat_all_str(h, t).acf(0)
	Next t
	Close #file



	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Pcorr."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1 ' по времени
		Print #file, dat_all_str(h, t).p_corr
	Next t
	Close #file



	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	Close #file


	For he = 0 To he_max Step he_step

		heCurrent = he

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,


		' вывод в файл значения гелия
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(CInt(Hkm(h)))+".txt" For Append As #file
		Print #file, He
		Close #file

		' Загружаем библиотеки АКФ
		Print "Загрузка библиотек АКФ... ";

		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf


		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) ' получаем список библиотек для He=0 и число этих библиотек

		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf

		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd

		Print_process_percent(1000)
		Print "OK"




		' очистка погрешностей
		For t = 0 To seans_num_out-1
			dat_all_str(h, t).d_c = 1e200
		Next t

		ranges_reset(h)


		' 1 шаг
		inverse_problem_v1_conv(h, z, Config_step_h_1, Config_step_te_1, Config_step_ti_1)

		' 2 шаг
		ranges_set(h, Config_range_h_2, Config_range_te_2, Config_range_ti_2)
		inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2) ' Step_Hyd = 2.5%, Step_Ti = 50K, Step_Te = 50K

		' 3 шаг
		ranges_set(h, Config_range_h_3, Config_range_te_3, Config_range_ti_3)
		inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3) ' Step_Hyd = 1%, Step_Ti = 20K, Step_Te = 20K

		results_write(h, he)

	Next he

	intervals_input_auto(h)

	draw_all(h, z)


	' запись значения обработанной высоты в файл
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Append As #file
	Print #file, Str(CInt(Hkm(h)))
	Close #file


	ranges_reg_set(h) ' регуляризация

	z += 1

	He_max = getHeMax( CInt(Hkm(h)) ) ' He_max2

	If Config_oxygen <> 0 Then
		he_max = 0
	EndIf

Next h ' !!!!

Print "OK"




Print
Print "OK"
break

'''==============================================








Sub inverse_problem_v1_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)

	Dim As Integer hyd
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d

	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)



	For hyd = 0 To libraries_num-1 Step step_hyd

		For te = 500 To 4000 Step step_te

			For ti = 500 To 4000 Step step_ti

				If (te/ti <= 3.5) And (te/ti >= 0.5) Then

					If prz_acf_fortran_conv_short_663( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), 19) <> 0 Then

						For t = 0 To seans_num_out-1 ' по времени

							If ( te >= dat_all_str(h, t).te_start ) And ( te <= dat_all_str(h, t).te_end ) And ( ti >= dat_all_str(h, t).ti_start ) And ( ti <= dat_all_str(h, t).ti_end ) And ( hyd >= dat_all_str(h, t).hyd_start ) And ( hyd <= dat_all_str(h, t).hyd_end ) Then

								If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) And ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) And ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then

'									If heCurrent <= heRange(t) Then

										d = 0
										For tau = 1 To 18
											d += coeffTau(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
										Next tau

										If d < dat_all_str(h, t).d_c Then
											dat_all_str(h, t).d_c = d
											dat_all_str(h, t).ti_c = ti
											dat_all_str(h, t).te_c = te
											dat_all_str(h, t).hyd_c = hyd
										EndIf

'									Endif

								EndIf

							EndIf

						Next t

					EndIf

				EndIf

			Next ti

		Next te

	Next hyd


End Sub


''' ================================================================





Sub inverse_problem_v2_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)

	Dim As Integer hyd
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d

	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)

	For hyd = 0 To libraries_num-1 Step step_hyd

		For t = 0 To seans_num_out-1 ' по времени

'			If heCurrent <= heRange(t) Then

				If ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then

					If (hyd >= dat_all_str(h, t).hyd_start) And (hyd <= dat_all_str(h, t).hyd_end) Then

						For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te

							If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) Then

								For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti

									If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) Then

										If (te/ti <= 3.5) And (te/ti >= 0.5) Then

											If prz_acf_fortran_conv_short_663( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), 19) <> 0 Then

												d = 0
												For tau = 1 To 18
													d += coeffTau(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
												Next tau

												If d < dat_all_str(h, t).d_c Then
													dat_all_str(h, t).d_c = d
													dat_all_str(h, t).ti_c = ti
													dat_all_str(h, t).te_c = te
													dat_all_str(h, t).hyd_c = hyd
												EndIf

											EndIf

										EndIf

									EndIf

								Next ti

							EndIf

						Next te

					EndIf

				EndIf

'			EndIf

		Next t

	Next hyd

End Sub


''' ================================================================

Sub ranges_set(ByVal h As Integer, ByVal delta_hyd As Integer, ByVal delta_te As Integer, ByVal delta_ti As Integer)

	Dim As Integer t

	For t = 0 To seans_num_out-1
		dat_all_str(h, t).te_start = dat_all_str(h, t).te_c-delta_te
		dat_all_str(h, t).te_end = dat_all_str(h, t).te_c+delta_te

		dat_all_str(h, t).ti_start = dat_all_str(h, t).ti_c-delta_ti
		dat_all_str(h, t).ti_end = dat_all_str(h, t).ti_c+delta_ti

		dat_all_str(h, t).hyd_start = dat_all_str(h, t).hyd_c-delta_hyd
		dat_all_str(h, t).hyd_end = dat_all_str(h, t).hyd_c+delta_hyd
	Next t

End Sub

''' ================================================================

Sub ranges_reset(ByVal h As Integer, t_start As Integer = -1 , t_end As Integer = -1)

	Dim As Integer t

	If t_start = -1 And t_end = -1 Then
		t_start = 0
		t_end = seans_num_out-1
	EndIf

	For t = t_start To t_end
		dat_all_str(h, t).te_start = 500
		dat_all_str(h, t).te_end = 4000

		dat_all_str(h, t).ti_start = 500
		dat_all_str(h, t).ti_end = 4000

		dat_all_str(h, t).hyd_start = 0
		dat_all_str(h, t).hyd_end = libraries_num-1

		dat_all_str(h, t).d_c = 1e200
	Next t

End Sub


''' ================================================================

Sub ranges_reg_set(ByVal h As Integer)

	Dim As Double RegMx
	Dim As Double RegSigma
	Dim As Double RegArray(0 To RegWnd-1)

	Dim As Integer t
	Dim As Integer i


	' пересчёт диапазона регуляризации для Te
	For t = 0 To seans_num_out-1-RegWnd+1

		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).te_c
		Next i

		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(0, t+RegWnd/2) = RegMx - DeltaTeMinus - RegN*RegSigma
		RegRange(1, t+RegWnd/2) = RegMx + DeltaTePlus  + RegN*RegSigma

	Next t

	' заполнение начала массива
	For t = 0 To RegWnd/2-1
		RegRange(0, t) = RegRange(0, RegWnd/2)
		RegRange(1, t) = RegRange(1, RegWnd/2)
	Next t

	' заполнение конца массива
	For i = 0 To RegWnd/2-1
		RegRange(0, seans_num_out-1-i) = RegRange(0, seans_num_out-1-RegWnd/2)
		RegRange(1, seans_num_out-1-i) = RegRange(1, seans_num_out-1-RegWnd/2)
	Next i





	' пересчёт диапазона регуляризации для Ti
	For t = 0 To seans_num_out-1-RegWnd+1

		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).ti_c
		Next i

		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(2, t+RegWnd/2) = RegMx - DeltaTiMinus - RegN*RegSigma
		RegRange(3, t+RegWnd/2) = RegMx + DeltaTiPlus  + RegN*RegSigma
	Next t

	' заполнение начала массива
	For t = 0 To RegWnd/2-1
		RegRange(2, t) = RegRange(2, RegWnd/2)
		RegRange(3, t) = RegRange(3, RegWnd/2)
	Next t

	' заполнение конца массива
	For i = 0 To RegWnd/2-1
		RegRange(2, seans_num_out-1-i) = RegRange(2, seans_num_out-1-RegWnd/2)
		RegRange(3, seans_num_out-1-i) = RegRange(3, seans_num_out-1-RegWnd/2)
	Next i




	' пересчёт диапазона регуляризации для Hyd
	For t = 0 To seans_num_out-1-RegWnd+1

		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).hyd_c
		Next i

		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(4, t+RegWnd/2) = RegMx - DeltaHydMinus - RegN*RegSigma
		RegRange(5, t+RegWnd/2) = RegMx + DeltaHydPlus  + RegN*RegSigma
	Next t

	' заполнение начала массива
	For t = 0 To RegWnd/2-1
		RegRange(4, t) = RegRange(4, RegWnd/2)
		RegRange(5, t) = RegRange(5, RegWnd/2)
	Next t

	' заполнение конца массива
	For i = 0 To RegWnd/2-1
		RegRange(4, seans_num_out-1-i) = RegRange(4, seans_num_out-1-RegWnd/2)
		RegRange(5, seans_num_out-1-i) = RegRange(5, seans_num_out-1-RegWnd/2)
	Next i

End Sub

''' ================================================================

Sub results_write(ByVal h As Integer, ByVal He As Integer)

	Dim As Integer file
	Dim As Integer t

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; dat_all_str(h, t).hyd_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; dat_all_str(h, t).ti_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; dat_all_str(h, t).te_c
	Next t
	Close #file


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).d_c
	Next t
	Close #file

	If He = -1 Then

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
		For t = 0 To seans_num_out-1
			Print #file, Using "###.# "; dat_all_str(h, t).he_c
		Next t
		Close #file

	EndIf


End Sub


''' ================================================================


Sub results_write2(ByVal h As Integer, ByVal He As Integer)

	Dim As Integer file
	Dim As Integer t

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; dat_all_str(h, t).hyd_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; dat_all_str(h, t).ti_c
	Next t
	Close #file

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; dat_all_str(h, t).te_c
	Next t
	Close #file


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).d_c
	Next t
	Close #file

	If He = -1 Then

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
		For t = 0 To seans_num_out-1
			Print #file, Using "###.# "; dat_all_str(h, t).he_c
		Next t
		Close #file

	EndIf


End Sub





''' ================================================================



Sub intervals_input(ByVal h As Integer)

	Dim As Integer i_left, i_right, He
	Dim As Integer file
	Dim As Integer t
	Dim As Double tmp_d

	Cls
	Print "Ввод интервалов..."

	Do

		Print
		Print h, Hkm(h)
		Print "Введите левую границу интервала (от 0 до "; seans_num_out-1; " ): ";
		Input "", i_left

		Print "Введите правую границу интервала (от "; i_left;" до "; seans_num_out-1; " ): ";
		Input "", i_right

		Print "Введите значение He+ (в %): ";
		Input "", He

		If (i_right >= i_left) And (i_left >= 0) And (i_right <= (seans_num_out-1) ) Then

			If (He >= 0) And (He <= 100) Then

				' заполнить He
				For t = i_left To i_right
					dat_all_str(h, t).he_c = He
				Next t

				' считать Te
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file

				For t = 0 To i_left-1
					Input #file, tmp_d
				Next

				For t = i_left To i_right
					Input #file, dat_all_str(h, t).te_c
				Next t

				Close #file

				' считать Ti
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file

				For t = 0 To i_left-1
					Input #file, tmp_d
				Next

				For t = i_left To i_right
					Input #file, dat_all_str(h, t).ti_c
				Next t

				Close #file

				' считать Hyd
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file

				For t = 0 To i_left-1
					Input #file, tmp_d
				Next

				For t = i_left To i_right
					Input #file, dat_all_str(h, t).hyd_c
				Next t

				Close #file

				' считать d_c
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file

				For t = 0 To i_left-1
					Input #file, tmp_d
				Next

				For t = i_left To i_right
					Input #file, dat_all_str(h, t).d_c
				Next t

				Close #file


			Else

				Color 12
				Print "Значение He+ должно быть в пределах от 0 до 100%!"
				Color 15

			EndIf

		Else

			Color 12
			Print "Неверно введены границы интервала!"
			Color 15

		EndIf

		Print "Продолжить ввод интервалов? (y/n) ";

	Loop Until GetKey_YN() = 0

	results_write(h, -1)

End Sub



''' ================================================================



Sub intervals_input_auto2(ByVal h As Integer)

	Dim As Integer t
	ReDim As Integer He_num(0 To He_max)
	Dim As Integer he, he_c
	Dim As Integer file
	Dim As Double d_c

	ReDim As Double d_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Ti_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Te_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Hyd_loaded(0 To He_max, 0 To seans_num_out-1)


	' загрузка временного хода Ti
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода Te
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода H+
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода погрешностей
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, d_loaded(he, t)
		Next t
		Close #file
	Next he


	For t = 0 To seans_num_out-1

		d_c = 1e200
		For he = 0 To He_max Step he_step
			If d_loaded(he, t) < d_c Then
				d_c = d_loaded(he, t)
				he_c = he
			EndIf
		Next he

		dat_all_str(h, t).he_c  = he_c
		dat_all_str(h, t).hyd_c = Hyd_loaded(he_c, t)
		dat_all_str(h, t).ti_c  = Ti_loaded(he_c, t)
		dat_all_str(h, t).te_c  = Te_loaded(he_c, t)
		dat_all_str(h, t).d_c   =  d_loaded(he_c, t)

	Next t

	results_write2(h, -1)

End Sub




''' ================================================================




Sub intervals_input_auto(ByVal h As Integer)

	Dim As Integer t, offset
	ReDim As Integer He_num(0 To He_max)
	Dim As Integer he, he_c
	Dim As Integer file
	Dim As Double d_c

	ReDim As Double d_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Ti_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Te_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Hyd_loaded(0 To He_max, 0 To seans_num_out-1)


	' загрузка временного хода Ti
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода Te
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода H+
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_loaded(he, t)
		Next t
		Close #file
	Next he

	' загрузка временного хода погрешностей
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, d_loaded(he, t)
		Next t
		Close #file
	Next he


	For t = 0 To seans_num_out-1

		d_c = 1e200
		For he = 0 To He_max Step he_step
			If d_loaded(he, t) < d_c Then
				d_c = d_loaded(he, t)
				he_c = he
			EndIf
		Next he

		dat_all_str(h, t).he_c  = he_c
		dat_all_str(h, t).hyd_c = Hyd_loaded(he_c, t)
		dat_all_str(h, t).ti_c  = Ti_loaded(he_c, t)
		dat_all_str(h, t).te_c  = Te_loaded(he_c, t)
		dat_all_str(h, t).d_c   =  d_loaded(he_c, t)

	Next t

	results_write(h, -1)

End Sub


''' ================================================================

Sub draw_all(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer wnd_trand_width = 21
	Dim As Integer offset

	Dim As Integer he, t, file

	ReDim As Double   t_loaded(0 To seans_num_out-1)

	ReDim As Double   d_he(0 To He_max, 0 To seans_num_out-1)

	ReDim As Double   d_loaded(0 To seans_num_out-1)
	ReDim As Double  ti_loaded(0 To seans_num_out-1)
	ReDim As Double  te_loaded(0 To seans_num_out-1)
	ReDim As Double hyd_loaded(0 To seans_num_out-1)
	ReDim As Double  he_loaded(0 To seans_num_out-1)


	ReDim As Double  ti_loaded_prev(0 To seans_num_out-1)
	ReDim As Double  te_loaded_prev(0 To seans_num_out-1)
	ReDim As Double hyd_loaded_prev(0 To seans_num_out-1)
	ReDim As Double  he_loaded_prev(0 To seans_num_out-1)

	ReDim As Double  ti_loaded_saved(0 To seans_num_out-1)
	ReDim As Double  te_loaded_saved(0 To seans_num_out-1)
	ReDim As Double hyd_loaded_saved(0 To seans_num_out-1)
	ReDim As Double  he_loaded_saved(0 To seans_num_out-1)

	Dim As Integer isSaved = 0

	ReDim As Double  tmp_d(0 To seans_num_out-1)
	ReDim As Double  tmp_d_d(0 To He_max+1, 0 To seans_num_out-1)

	Dim As Double SCALE_Y = 140

	Dim As Integer key
	Dim As Integer SEL = 0

	Dim As Double max_val, min_val

	Dim As Integer wnd, point_start, point_end


	' загрузка времени
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file


	' загрузка временного хода погрешностей
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, d_he(he, t)
		Next t
		Close #file
	Next he

	param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

	If z <> 0 Then

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

		If Err() > 0 Then
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
		EndIf

		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)
		Next t
		Close #file

		array_trand_d(@tmp_d(0), @ti_loaded_prev(0), wnd_trand_width, seans_num_out)


		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

		If Err() > 0 Then
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
		EndIf

		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)
		Next t
		Close #file

		array_trand_d(@tmp_d(0), @te_loaded_prev(0), wnd_trand_width, seans_num_out)


		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

		If Err() > 0 Then
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
		EndIf

		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)
		Next t
		Close #file

		array_trand_d(@tmp_d(0), @hyd_loaded_prev(0), wnd_trand_width, seans_num_out)


		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

		If Err() > 0 Then
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
		EndIf

		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)
		Next t
		Close #file

		array_trand_d(@tmp_d(0), @he_loaded_prev(0), wnd_trand_width, seans_num_out)

	EndIf



	Do

		ScreenLock

		Cls


		If SEL = 1 Then
			For t = START_X To seans_num_out-1-1
				If t >= point_start And t <= point_end Then
					Line (1+t-START_X, 25 )-(1+t-START_X, 768-25 ), 8
				EndIf
			Next t
		EndIf

		' вывод погрешностей

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_he(he, t)-d_loaded(t)
			Next t
		Next he

		' поиск глобального минимума
		min_val = 1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) < min_val Then min_val = tmp_d_d(he, t)
			Next t
		Next he

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) -= min_val
			Next t
		Next he

		' поиск глобального максимума
		max_val = -1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) > max_val Then max_val = tmp_d_d(he, t)
			Next t
		Next he

		If max_val <> 0 Then
			For he = 0 To He_max Step he_step
				For t = 0 To seans_num_out-1
					tmp_d_d(he, t) /= max_val
				Next t
			Next he
		EndIf

		Line (0, 150 )-(1024, 150 ), 7, , &b0000000011110000
		For he = 0 To He_max Step he_step
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 150-SCALE_Y*tmp_d_d(he, t) )-(1+t+1-START_X, 150-SCALE_Y*tmp_d_d(he, t+1) ), 15-he\2
			Next t
		Next he
		Draw String (950, 135), Left( Str(d_loaded(CUR)), 12), 15



		' вывод Te

		If isSaved = 1 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = te_loaded_saved(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 300-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 300-SCALE_Y*tmp_d(t+1) ), 7
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) = te_loaded(t)
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 300 )-(1024, 300 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 300-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 300-SCALE_Y*tmp_d(t+1) ), 12
			Next t
			Draw String (950, 285), Left( Str(te_loaded(CUR)), 12), 12

		Else

			For t = 0 To seans_num_out-1
				tmp_d(t) = te_loaded(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 300 )-(1024, 300 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 300-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 300-SCALE_Y*tmp_d(t+1) ), 12
			Next t
			Draw String (950, 285), Left( Str(te_loaded(CUR)), 12), 12

		EndIf

		If z <> 0 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = (te_loaded_prev(t)-min_val)
				If max_val <> 0 Then
					tmp_d(t) /= max_val
				EndIf
			Next t

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 300-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 300-SCALE_Y*tmp_d(t+1) ), 4
			Next t
			Draw String (950, 305), Left( Str(Int(te_loaded_prev(CUR))), 12), 4

		EndIf


		' вывод H+

		If isSaved = 1 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = hyd_loaded_saved(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 450-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 450-SCALE_Y*tmp_d(t+1) ), 7
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) = hyd_loaded(t)
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 450 )-(1024, 450 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 450-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 450-SCALE_Y*tmp_d(t+1) ), 13
			Next t
			Draw String (950, 435), Left( Str(hyd_loaded(CUR)/2.0), 12), 13

		Else

			For t = 0 To seans_num_out-1
				tmp_d(t) = hyd_loaded(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 450 )-(1024, 450 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 450-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 450-SCALE_Y*tmp_d(t+1) ), 13
			Next t
			Draw String (950, 435), Left( Str(hyd_loaded(CUR)/2.0), 12), 13

		EndIf

		If z <> 0 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = (hyd_loaded_prev(t)-min_val)
				If max_val <> 0 Then
					tmp_d(t) /= max_val
				EndIf
			Next t

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 450-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 450-SCALE_Y*tmp_d(t+1) ), 5
			Next t
			Draw String (950, 455), Left( Str(hyd_loaded_prev(CUR)/2.0), 4), 5

		EndIf


		' вывод He+


		If isSaved = 1 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = he_loaded_saved(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 600-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 600-SCALE_Y*tmp_d(t+1) ), 7
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) = he_loaded(t)
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 600 )-(1024, 600 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 600-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 600-SCALE_Y*tmp_d(t+1) ), 14
			Next t
			Draw String (950, 585), Left( Str(he_loaded(CUR)), 12), 14

		Else

			For t = 0 To seans_num_out-1
				tmp_d(t) = he_loaded(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 600 )-(1024, 600 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 600-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 600-SCALE_Y*tmp_d(t+1) ), 14
			Next t
			Draw String (950, 585), Left( Str(he_loaded(CUR)), 12), 14

		EndIf

		If z <> 0 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = (he_loaded_prev(t)-min_val)
				If max_val <> 0 Then
					tmp_d(t) /= max_val
				EndIf
			Next t

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 600-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 600-SCALE_Y*tmp_d(t+1) ), 6
			Next t
			Draw String (950, 605), Left( Str( he_loaded_prev(CUR) ), 4), 6

		EndIf


		' вывод Ti

		If isSaved = 1 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = ti_loaded_saved(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 750-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 750-SCALE_Y*tmp_d(t+1) ), 7
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) = ti_loaded(t)
			Next t

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 750 )-(1024, 750 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 750-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 750-SCALE_Y*tmp_d(t+1) ), 11
			Next t
			Draw String (950, 735), Left( Str(ti_loaded(CUR)), 12), 11

		Else

			For t = 0 To seans_num_out-1
				tmp_d(t) = ti_loaded(t)
			Next t

			min_val = array_min_d (@tmp_d(0), seans_num_out)

			For t = 0 To seans_num_out-1
				tmp_d(t) -= min_val
			Next t

			max_val = array_max_d (@tmp_d(0), seans_num_out)
			If max_val <> 0 Then
				array_norm_d(@tmp_d(0), @tmp_d(0), max_val, seans_num_out)
			EndIf

			Line (0, 750 )-(1024, 750 ), 7, , &b0000000011110000
			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 750-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 750-SCALE_Y*tmp_d(t+1) ), 11
			Next t
			Draw String (950, 735), Left( Str(ti_loaded(CUR)), 12), 11

		EndIf


		If z <> 0 Then

			For t = 0 To seans_num_out-1
				tmp_d(t) = (ti_loaded_prev(t)-min_val)
				If max_val <> 0 Then
					tmp_d(t) /= max_val
				EndIf
			Next t

			For t = START_X To seans_num_out-1-1
				Line (1+t-START_X, 750-SCALE_Y*tmp_d(t) )-(1+t+1-START_X, 750-SCALE_Y*tmp_d(t+1) ), 2
			Next t
			Draw String (950, 755), Left( Str(Int(ti_loaded_prev(CUR))), 12), 2

		EndIf


		Line (1+CUR-START_X, 25)-(1+CUR-START_X, 768-25), 7

		Draw String (5, 5),   "n="+Left( Str(CUR ), 6), 14
		Draw String (100, 5), "t="+Left( Str(t_loaded(CUR)), 6), 15


		Draw String (200, 5), "q="+Left(Str(dat_all_str(h, CUR).q), 6), 11

		Draw String (300, 5), "h="+Left(Str( CInt(Hkm(h)) ), 4)+" км", 12

		If isSaved = 1 Then
			Draw String (500, 5), "*", 13
		EndIf

		Draw String (900, 5), "F1 - помощь", 4


		ScreenUnLock()

		key = GetKey()

		Select Case key

			Case KEY_CTRL_Q
				Cls
				Color 12
				Print "Выйти из программы? (y/n) "
				If GetKey_YN() = 1 Then
					End(0)
				EndIf
				Color 15

			Case KEY_F1
				draw_help()

			Case KEY_CTRL_LEFT
				If START_X < seans_num_out-1-1 Then START_X += 1 End If

			Case KEY_CTRL_RIGHT
				If START_X >0 Then START_X -= 1  End If

			Case KEY_RIGHT
				If CUR < seans_num_out-1 Then CUR += 1 End If
				If SEL = 1 Then
					If point_end < seans_num_out-1 Then
						point_end += 1
					EndIf
				EndIf

			Case KEY_LEFT

				If SEL = 1 Then
					If point_end > point_start Then
						point_end -= 1
						If CUR > 0 Then CUR -= 1  End If
					EndIf
				Else
					If CUR > 0 Then CUR -= 1  End If
				EndIf

			Case KEY_HOME
				If SEL = 0 Then
					CUR=START_X
				EndIf


			Case KEY_END
				If SEL = 0 Then
					If START_X+1024-1 < seans_num_out Then
						CUR = START_X+1024-1-3
					Else
						CUR=seans_num_out-1
					EndIf
				EndIf

			Case KEY_TAB
				If SEL = 0 Then
					If CUR + 10 < seans_num_out-1 Then CUR += 10
				EndIf

			Case KEY_1
				draw_d(h, z)

			Case KEY_SPACE
				If SEL = 1 Then SEL = 0 Else SEL = 1
				point_start = CUR
				point_end = CUR

			Case KEY_I
				If SEL = 1 Then

					If point_end-point_start = 0 Then
						Exit Select
					EndIf

					Input "Введите номер параметра (0 - отмена, 1 - Te, 2 - H+, 3 - He+, 4 - Ti, 5 - всё): ", wnd

					If wnd = 0 Then
						Exit Select
					EndIf

					save(h)
					Select Case wnd

						Case 1
							interpolate_param(h, z, point_start, point_end, PARAM_TE)

						Case 2
							interpolate_param(h, z, point_start, point_end, PARAM_H)

						Case 3
							interpolate_param(h, z, point_start, point_end, PARAM_HE)

						Case 4
							interpolate_param(h, z, point_start, point_end, PARAM_TI)

						Case 5
							interpolate_param(h, z, point_start, point_end, PARAM_TI)
							interpolate_param(h, z, point_start, point_end, PARAM_TE)
							interpolate_param(h, z, point_start, point_end, PARAM_H)
							interpolate_param(h, z, point_start, point_end, PARAM_HE)

					End Select

					param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
					SEL = 0

				EndIf



			Case KEY_U
				isSaved = 0
				undo(h)
				param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )


			Case KEY_R


				Input "Введите номер параметра (0 - отмена, 1 - Te, 2 - H+, 3 - He+, 4 - Ti, 5 - Te и Ti, 6 - H+ и Te и Ti): ", wnd

				If wnd = 0 Then
					Exit Select
				EndIf

				Select Case wnd

					Case 1
						inverse_problem_te(h, z)
						param_load2(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

					Case 2
						inverse_problem_hyd(h, z)
						param_load2(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

					Case 3
						If SEL = 1 Then

							If point_end-point_start < 1 Then
								Exit Select
							EndIf

							save(h)
							inverse_problem_he(h, z, point_start, point_end)

							param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
							SEL = 0

						EndIf

					Case 4
						inverse_problem_ti(h, z)
						param_load2(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

					Case 5
						inverse_problem_ti_te(h, z)
						param_load2(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

					Case 6
						inverse_problem_hyd_ti_te(h, z)
						param_load2(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

				End Select


			Case KEY_P
				If SEL = 1 Then

					Input "Введите номер параметра (0 - отмена, 3 - He+): ", wnd

					If wnd = 0 Then
						Exit Select
					EndIf

					save(h)

					Select Case wnd
						Case 3
							input_param(h, z, point_start, point_end, PARAM_HE)
					End Select

					param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
					SEL = 0

				EndIf

			Case KEY_T
				Input "Введите ширину окна для усреднения по времени: ", wnd_trand_width
				If wnd_trand_width Mod 2 = 0 Then
					wnd_trand_width += 1
				EndIf

				If z <> 0 Then

					file = FreeFile()
					Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

					If Err() > 0 Then
						file = FreeFile()
						Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
					EndIf

					For t = 0 To seans_num_out-1
						Input #file, tmp_d(t)
					Next t
					Close #file

					array_trand_d(@tmp_d(0), @ti_loaded_prev(0), wnd_trand_width, seans_num_out)


					file = FreeFile()
					Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

					If Err() > 0 Then
						file = FreeFile()
						Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
					EndIf

					For t = 0 To seans_num_out-1
						Input #file, tmp_d(t)
					Next t
					Close #file

					array_trand_d(@tmp_d(0), @te_loaded_prev(0), wnd_trand_width, seans_num_out)


					file = FreeFile()
					Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

					If Err() > 0 Then
						file = FreeFile()
						Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
					EndIf

					For t = 0 To seans_num_out-1
						Input #file, tmp_d(t)
					Next t
					Close #file

					array_trand_d(@tmp_d(0), @hyd_loaded_prev(0), wnd_trand_width, seans_num_out)


					file = FreeFile()
					Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file

					If Err() > 0 Then
						file = FreeFile()
						Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h-Hstep)))+".txt" For Input As #file
					EndIf

					For t = 0 To seans_num_out-1
						Input #file, tmp_d(t)
					Next t
					Close #file

					array_trand_d(@tmp_d(0), @he_loaded_prev(0), wnd_trand_width, seans_num_out)

				EndIf


			Case KEY_S
				isSaved = 1
				save(h)

				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				For t = 0 To seans_num_out-1
					Input #file, te_loaded_saved(t)
				Next t
				Close #file

				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				For t = 0 To seans_num_out-1
					Input #file, ti_loaded_saved(t)
				Next t
				Close #file

				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				For t = 0 To seans_num_out-1
					Input #file, he_loaded_saved(t)
				Next t
				Close #file

				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				For t = 0 To seans_num_out-1
					Input #file, hyd_loaded_saved(t)
				Next t
				Close #file


			Case KEY_CTRL_A
				point_start = 0
				point_end = seans_num_out-1
				CUR=seans_num_out-1
				SEL = 1


			Case KEY_M

				Dim As Integer mode

				If SEL = 1 Then

					If point_end-point_start < 1 Then
						Exit Select
					EndIf

					Input "Введите номер параметра (0 - отмена, 1 - Te, 2 - H+, 4 - Ti): ", wnd

					If wnd = 0 Then
						Exit Select
					EndIf

					If wnd = 4 Then
						Input "Введите номер режима (0 - отмена, 1 - задать значение, 2 - усреднение, 3 - линейная интерполяция, 4 - аппроксимация полиномом,  5 - аппроксимация сплайнами со скользящим окном, 6 - градиент, 7 - Ti=Te (если Ti>Te)): ", mode
					Else
						Input "Введите номер режима (0 - отмена, 1 - задать значение, 2 - усреднение, 3 - линейная интерполяция, 4 - аппроксимация полиномом,  5 - аппроксимация сплайнами со скользящим окном, 6 - градиент): ", mode
					EndIf

					If mode = 0 Then
						Exit Select
					EndIf

					Select Case wnd

						Case 1
							Select Case mode

								Case 1
									Dim As Integer level
									Input "Введите значение параметра: ", level
									level_te(h, z, point_start, point_end, level)

								Case 2
									Dim As Integer wnd_width
									Input "Введите ширину окна: ", wnd_width
									trand_te(h, z, point_start, point_end, wnd_width)

								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_TE)

								Case 4
									Dim As Integer degree
									Input "Введите степень полинома: ", degree
									poly_te(h, z, point_start, point_end, degree)

								Case 5
									Dim As Integer wnd_width, degree
									Input "Введите ширину окна: ", wnd_width
									Input "Введите степень полинома: ", degree
									spline_wnd_te(h, z, point_start, point_end, wnd_width, degree)

								Case 6
									gradient_te(h, z, point_start, point_end)

							End Select

						Case 2
							Select Case mode
								Case 1
									Dim As Integer level
									Input "Введите значение параметра (%): ", level
									level_hyd(h, z, point_start, point_end, level)

								Case 2
									Dim As Integer wnd_width
									Input "Введите ширину окна: ", wnd_width
									trand_hyd(h, z, point_start, point_end, wnd_width)

								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_H)

								Case 4
									Dim As Integer degree
									Input "Введите степень полинома: ", degree
									poly_hyd(h, z, point_start, point_end, degree)

							End Select

						Case 4
							Select Case mode

								Case 1
									Dim As Integer level
									Input "Введите значение параметра: ", level
									level_ti(h, z, point_start, point_end, level)

								Case 2
									Dim As Integer wnd_width
									Input "Введите ширину окна: ", wnd_width
									trand_ti(h, z, point_start, point_end, wnd_width)

								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_TI)

								Case 4
									Dim As Integer degree
									Input "Введите степень полинома: ", degree
									poly_ti(h, z, point_start, point_end, degree)

								Case 5
									Dim As Integer wnd_width, degree
									Input "Введите ширину окна: ", wnd_width
									Input "Введите степень полинома: ", degree
									spline_wnd_ti(h, z, point_start, point_end, wnd_width, degree)

								Case 7
									equate_ti(h, z, point_start, point_end)

							End Select


					End Select

					param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
					SEL = 0

				EndIf



		End Select

		If key = KEY_CTRL_N Then Exit Do End If

	Loop

End Sub

''' ================================================================

Sub draw_d(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer wnd_trand_width

	Dim As Integer he, t, file

	ReDim As Double   he_loaded(0 To seans_num_out-1)

	ReDim As Double   t_loaded(0 To seans_num_out-1)

	ReDim As Double   d_loaded(0 To He_max+1, 0 To seans_num_out-1)

	ReDim As Double  tmp_d(0 To seans_num_out-1)
	ReDim As Double  tmp_d_d(0 To He_max+1, 0 To seans_num_out-1)

	Dim As Double SCALE_Y = 750
	'	Dim As Integer START_X = 0


	Dim As Integer key
	Dim As Integer offset

	Dim As Double max_val, min_val

	'	Dim As Integer CUR = 0
	Dim As Integer CUR_HE

	Cls
	Input "Введите ширину окна: ", wnd_trand_width


	' загрузка времени
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file



	' загрузка временного хода He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he_loaded(t)
	Next t
	Close #file



	' загрузка временного хода погрешностей
	For he = 0 To He_max Step he_step

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)'d_loaded(he, t)
		Next t
		Close #file

		array_trand_d( @tmp_d(0), @d_loaded(he, 0), wnd_trand_width, seans_num_out)

	Next he

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, tmp_d(t)'d_loaded(He_max+1, t)
	Next t
	Close #file

	array_trand_d( @tmp_d(0), @d_loaded(He_max+1, 0), wnd_trand_width, seans_num_out)


	Do

		ScreenLock()

		Cls()

		' вывод погрешностей

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_loaded(he, t)-d_loaded(He_max+1, t)
			Next t
		Next he

		' поиск глобального минимума
		min_val = 1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) < min_val Then min_val = tmp_d_d(he, t)
			Next t
		Next he

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) -= min_val
			Next t
		Next he

		' поиск глобального максимума
		max_val = -1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) > max_val Then max_val = tmp_d_d(he, t)
			Next t
		Next he



		If max_val <> 0 Then
			For he = 0 To He_max Step he_step
				For t = 0 To seans_num_out-1
					tmp_d_d(he, t) /= max_val
				Next t
			Next he
		EndIf

		Line (0, 740 )-(1024, 740 ), 7, , &b0000000011110000
		For he = 0 To He_max Step he_step
			For t = START_X To seans_num_out-1-1
				Line (t-START_X, 740-SCALE_Y*tmp_d_d(he, t) )-(t+1-START_X, 740-SCALE_Y*tmp_d_d(he, t+1) ), 15-he\2
			Next t
		Next he

		Line (CUR-START_X, 2)-(CUR-START_X, 768-2), 7



		min_val = 1e200
		For he = 0 To He_max Step he_step
			If tmp_d_d(he, CUR) < min_val Then
				min_val = tmp_d_d(he, CUR)
				CUR_HE = he
			EndIf
		Next he

		Line (0, 0)-(1024, 25), 0, BF
		Draw String (5, 5), Left( Str(t_loaded(CUR)), 6), 15
		Draw String (100, 5), "He = " + Left( Str( CUR_HE ), 3) + "%", 15-CUR_HE\2
		Draw String (200, 5), "He = " + Left( Str( he_loaded(CUR) ), 3)+"%", 15-he_loaded(CUR)\2
		Draw String (300, 5), "|б| = " + Left( Str( Abs(he_loaded(CUR)-CUR_HE) ), 3)+"%", 11

		ScreenUnLock

		key = GetKey

		Select Case key

			Case KEY_CTRL_LEFT
				If START_X < seans_num_out-1-1 Then START_X += 1 End If

			Case KEY_CTRL_RIGHT
				If START_X >0 Then START_X -= 1  End If

			Case KEY_RIGHT
				If CUR < seans_num_out-1 Then CUR += 1 End If

			Case KEY_LEFT
				If CUR > 0 Then CUR -= 1  End If

			Case KEY_HOME
				CUR=START_X

			Case KEY_END
				If START_X+1024-1 < seans_num_out Then
					CUR = START_X+1024-1-3
				Else
					CUR=seans_num_out-1
				End If

			Case KEY_TAB
				If CUR + 10 < seans_num_out-1 Then CUR += 10

			Case KEY_UP
				If SCALE_Y < 384000 Then SCALE_Y *= 2

			Case KEY_DOWN
				If SCALE_Y > 750 Then SCALE_Y \= 2


		End Select

		If key = KEY_ESC Then Exit Do End If

	Loop

End Sub



''' ================================================================

Sub draw_te(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer wnd_trand_width

	Dim As Integer t, file

	ReDim As Double   he_loaded(0 To seans_num_out-1)

	ReDim As Double   t_loaded(0 To seans_num_out-1)

	ReDim As Double   d_loaded(0 To He_max+1, 0 To seans_num_out-1)

	ReDim As Double  tmp_d(0 To seans_num_out-1)
	ReDim As Double  tmp_d_d(0 To He_max+1, 0 To seans_num_out-1)

	Dim As Integer he

	Dim As Double SCALE_Y = 750
	'	Dim As Integer START_X = 0


	Dim As Integer key
	Dim As Integer offset

	Dim As Double max_val, min_val

	'	Dim As Integer CUR = 0
	Dim As Integer CUR_HE

	Cls
	Input "Введите ширину окна: ", wnd_trand_width


	' загрузка времени
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file



	' загрузка временного хода He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he_loaded(t)
	Next t
	Close #file



	' загрузка временного хода погрешностей
	For he = 0 To He_max Step he_step

		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)'d_loaded(he, t)
		Next t
		Close #file

		array_trand_d( @tmp_d(0), @d_loaded(he, 0), wnd_trand_width, seans_num_out)

	Next he

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, tmp_d(t)'d_loaded(He_max+1, t)
	Next t
	Close #file

	array_trand_d( @tmp_d(0), @d_loaded(He_max+1, 0), wnd_trand_width, seans_num_out)


	Do

		ScreenLock

		Cls

		' вывод погрешностей

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_loaded(he, t)-d_loaded(He_max+1, t)
			Next t
		Next he

		' поиск глобального минимума
		min_val = 1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) < min_val Then min_val = tmp_d_d(he, t)
			Next t
		Next he

		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) -= min_val
			Next t
		Next he

		' поиск глобального максимума
		max_val = -1e200
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				If tmp_d_d(he, t) > max_val Then max_val = tmp_d_d(he, t)
			Next t
		Next he



		If max_val <> 0 Then
			For he = 0 To He_max Step he_step
				For t = 0 To seans_num_out-1
					tmp_d_d(he, t) /= max_val
				Next t
			Next he
		EndIf

		Line (0, 740 )-(1024, 740 ), 7, , &b0000000011110000
		For he = 0 To He_max Step he_step
			For t = START_X To seans_num_out-1-1
				Line (t-START_X, 740-SCALE_Y*tmp_d_d(he, t) )-(t+1-START_X, 740-SCALE_Y*tmp_d_d(he, t+1) ), 15-he\2
			Next t
		Next he

		Line (CUR-START_X, 2)-(CUR-START_X, 768-2), 7



		min_val = 1e200
		For he = 0 To He_max Step he_step
			If tmp_d_d(he, CUR) < min_val Then
				min_val = tmp_d_d(he, CUR)
				CUR_HE = he
			EndIf
		Next he

		Line (0, 0)-(1024, 25), 0, BF
		Draw String (5, 5), Left( Str(t_loaded(CUR)), 6), 15
		Draw String (100, 5), "He = " + Left( Str( CUR_HE ), 3) + "%", 15-CUR_HE\2
		Draw String (200, 5), "He = " + Left( Str( he_loaded(CUR) ), 3)+"%", 15-he_loaded(CUR)\2
		Draw String (300, 5), "|б| = " + Left( Str( Abs(he_loaded(CUR)-CUR_HE) ), 3)+"%", 11

		ScreenUnLock()

		key = GetKey()

		Select Case key

			Case KEY_CTRL_LEFT
				If START_X < seans_num_out-1-1 Then START_X += 1 End If

			Case KEY_CTRL_RIGHT
				If START_X >0 Then START_X -= 1  End If

			Case KEY_RIGHT
				If CUR < seans_num_out-1 Then CUR += 1 End If

			Case KEY_LEFT
				If CUR > 0 Then CUR -= 1  End If

			Case KEY_HOME
				CUR=START_X

			Case KEY_END
				If START_X+1024-1 < seans_num_out Then
					CUR = START_X+1024-1-3
				Else
					CUR=seans_num_out-1
				End If

			Case KEY_TAB
				If CUR + 10 < seans_num_out-1 Then CUR += 10

			Case KEY_UP
				If SCALE_Y < 384000 Then SCALE_Y *= 2

			Case KEY_DOWN
				If SCALE_Y > 750 Then SCALE_Y \= 2


		End Select

		If key = KEY_ESC Then Exit Do End If

	Loop

End Sub



''' ================================================================



Sub undo(ByVal h As Integer)
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")

	Kill (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	Kill (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	Kill (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	Kill (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	Kill (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
End Sub



''' ================================================================



Sub save(ByVal h As Integer)

	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "_D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")

	Print
	Color 12
	Print "Сохранено..."
	Color 15
	Sleep 400

End Sub


''' ================================================================


Sub interpolate_param(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, ByVal param As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	Dim As Double  d_param

	Dim As String param_str

	Select Case param

		Case PARAM_TI
			param_str = "Ti."

		Case PARAM_TE
			param_str = "Te."

		Case PARAM_H
			param_str = "Hyd."

		Case PARAM_HE
			param_str = "He."

	End Select

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ param_str +Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	d_param = ( param_loaded(t_end)-param_loaded(t_start) ) / (t_end-t_start)


	If (param = PARAM_TI) Or (param = PARAM_TE) Then
		For t = t_start To t_end ' по времени
			param_loaded(t) =  ( ( CInt( param_loaded(t_start) + d_param*(t-t_start) ) ) \ 20 ) * 20
		Next t
	Else
		For t = t_start To t_end ' по времени
			param_loaded(t) =  CInt( param_loaded(t_start) + d_param*(t-t_start) )
		Next t
	EndIf


	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ param_str +Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	If param = PARAM_H Or param = PARAM_HE Then
		For t = 0 To seans_num_out-1
			Print #file, Using "###.# "; param_loaded(t)
		Next t
	Else
		For t = 0 To seans_num_out-1
			Print #file, Using "####.# "; param_loaded(t)
		Next t
	EndIf
	Close #file

End Sub


''' ================================================================


Sub trand_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)

	For t = t_start To t_end ' по времени
		param_loaded(t) =   ( CInt( trand(t) ) \ 20 ) * 20
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub


''' ================================================================


Sub trand_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)

	For t = t_start To t_end ' по времени
		param_loaded(t) =   ( CInt( trand(t) ) \ 20 ) * 20
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub


''' ================================================================


Sub trand_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)

	For t = t_start To t_end ' по времени
		param_loaded(t) =   CInt( trand(t) )
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; param_loaded(t)
	Next t
	Close #file

End Sub



''' ================================================================


Sub level_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	For t = t_start To t_end ' по времени
		param_loaded(t) =  ( CInt( level ) \ 20 ) * 20
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub


''' ================================================================


Sub level_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	For t = t_start To t_end ' по времени
		param_loaded(t) =  ( CInt( level ) \ 20 ) * 20
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub



''' ================================================================


Sub level_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, level As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file


	For t = t_start To t_end ' по времени
		param_loaded(t) = CInt( level*2 )
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub



''' ================================================================




Sub gradient_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)

	ReDim As Double param_loaded(0 To z, 0 To seans_num_out-1)
	ReDim As Double gradients(0 To z-1)

	Dim As Double gradient

	Dim As Integer t
	Dim As Integer file


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(z, t)
	Next t
	Close #file


	For zz As Integer = 1 To z
		' загрузка временного хода выбранного параметра
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te2."+ Str(-1)+"."+Str(CInt(Hkm(h-Hstep*(zz))))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, param_loaded(z-zz, t)
		Next t
		Close #file
	Next zz

	For zz As Integer = 0 To z-2
		gradients(zz) = 0
		For t = t_start To t_end ' по времени
			gradients(zz) += param_loaded(zz+1, t)-param_loaded(zz, t)
		Next t
		gradients(zz) /= t_end-t_start
	Next zz

	Cls

	For zz As Integer = 0 To z-2
		Print Using "#### "; gradients(zz);
	Next zz
	Print

	Input "Введите значение градиента: ", gradient

	For t = t_start To t_end ' по времени
		param_loaded(z, t) =  ( CInt( param_loaded(z-1, t) + gradient ) \ 20 ) * 20
	Next t

	' запись временного хода выбранного параметра

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(z, t)
	Next t
	Close #file

End Sub



''' ================================================================


Sub poly_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	ReDim As Double in_x(0 To (t_end-t_start))
	ReDim As Double in_y(0 To (t_end-t_start))
	ReDim As Double coeff(0 To degree)


	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file

	Dim As Integer y = 0
	For t = t_start To t_end ' по времени
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t

	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)

	y = 0
	For t = t_start To t_end ' по времени

		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k

		param_loaded(t) = ( CInt( param_loaded(t) ) \ 20 ) * 20

		y += 1

	Next t


	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub


''' ================================================================


Sub poly_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	ReDim As Double in_x(0 To (t_end-t_start))
	ReDim As Double in_y(0 To (t_end-t_start))
	ReDim As Double coeff(0 To degree)


	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file

	Dim As Integer y = 0
	For t = t_start To t_end ' по времени
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t

	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)

	y = 0
	For t = t_start To t_end ' по времени

		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k

		param_loaded(t) = ( CInt( param_loaded(t) ) \ 20 ) * 20

		y += 1

	Next t


	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub



''' ================================================================


Sub poly_hyd(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, degree As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	ReDim As Double in_x(0 To (t_end-t_start))
	ReDim As Double in_y(0 To (t_end-t_start))
	ReDim As Double coeff(0 To degree)


	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file

	Dim As Integer y = 0
	For t = t_start To t_end ' по времени
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t

	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)

	y = 0
	For t = t_start To t_end ' по времени

		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k

		param_loaded(t) = CInt( param_loaded(t) )

		y += 1

	Next t


	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; param_loaded(t)
	Next t
	Close #file

End Sub




''' ================================================================


Sub spline_wnd_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer, degree As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	ReDim As Double out_y(0 To t_end-t_start)

	ReDim As Double in_x(0 To wnd-1)
	ReDim As Double in_y(0 To wnd-1)
	ReDim As Double coeff(0 To degree)

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file



	Dim As Integer y = 0
	For t = t_start To t_end ' по времени

		Dim As Integer num = 0
		For tt As Integer = -wnd\2 To wnd\2

			Dim As Integer index = t+tt

			If index > 0 And index < seans_num_out-1 Then
				in_y(num) = param_loaded(index)
				in_x(num) = index
				num += 1
			EndIf

		Next tt

		approx_poly_coeff_d(@in_x(0), @in_y(0), num, @coeff(0), degree)

		out_y(y) = 0

		For k As Integer = 0 To degree
			out_y(y) += coeff(k) * t^k
		Next

		y += 1

	Next t

	y = 0
	For t = t_start+1 To t_end-1 ' по времени
		param_loaded(t) =   ( CInt( out_y(y) ) \ 20 ) * 20
		y += 1
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub



''' ================================================================


Sub spline_wnd_te(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, wnd As Integer, degree As Integer)

	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double trand(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	ReDim As Double out_y(0 To t_end-t_start)

	ReDim As Double in_x(0 To wnd-1)
	ReDim As Double in_y(0 To wnd-1)
	ReDim As Double coeff(0 To degree)

	' загрузка временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file



	Dim As Integer y = 0
	For t = t_start To t_end ' по времени

		Dim As Integer num = 0
		For tt As Integer = -wnd\2 To wnd\2

			Dim As Integer index = t+tt

			If index > 0 And index < seans_num_out-1 Then
				in_y(num) = param_loaded(index)
				in_x(num) = index
				num += 1
			EndIf

		Next tt

		approx_poly_coeff_d(@in_x(0), @in_y(0), num, @coeff(0), degree)

		out_y(y) = 0

		For k As Integer = 0 To degree
			out_y(y) += coeff(k) * t^k
		Next

		y += 1

	Next t

	y = 0
	For t = t_start+1 To t_end-1 ' по времени
		param_loaded(t) =   ( CInt( out_y(y) ) \ 20 ) * 20
		y += 1
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; param_loaded(t)
	Next t
	Close #file

End Sub




''' ================================================================

Sub inverse_problem_he(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)

	ReDim As Double   d_loaded(0 To seans_num_out-1)
	ReDim As Double  ti_loaded(0 To seans_num_out-1)
	ReDim As Double  te_loaded(0 To seans_num_out-1)
	ReDim As Double hyd_loaded(0 To seans_num_out-1)
	ReDim As Double  he_loaded(0 To seans_num_out-1)

	ReDim As Double   d_all(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double  ti_all(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double  te_all(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double hyd_all(0 To He_max, 0 To seans_num_out-1)

	Dim As Double d_he
	Dim As Integer t, he
	Dim As Integer file


	For he = 0 To He_max Step he_step

		' Загрузка Ti
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_all(he, t)
		Next t
		Close #file

		' Загрузка Te
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_all(he, t)
		Next t
		Close #file

		' Загрузка H+
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_all(he, t)
		Next t
		Close #file

		' Загрузка погрешностей
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, d_all(he, t)
		Next t
		Close #file

	Next he


	param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

	d_he = ( he_loaded(t_end)-he_loaded(t_start) ) / (t_end-t_start)


	For t = t_start To t_end ' по времени
		he_loaded(t) = CInt( he_loaded(t_start) + d_he*(t-t_start) )
	Next t


	For t = t_start+1 To t_end-1 ' по времени
		d_loaded(t)   = d_all  (he_loaded(t), t)
		ti_loaded(t)  = ti_all (he_loaded(t), t)
		te_loaded(t)  = te_all (he_loaded(t), t)
		hyd_loaded(t) = hyd_all(he_loaded(t), t)
	Next t


	param_save(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

End Sub


''' ================================================================


Sub inverse_problem_ti(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer he, hyd
	Dim As Integer t
	Dim As Integer file
	Dim As Double  ti_loaded(0 To seans_num_out-1)

	Cls()

	' Загрузка Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file

	For he = 0 To he_max Step he_step

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,

		' Загружаем библиотеки АКФ

		Print "Загрузка библиотек АКФ... ";

		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf

		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) ' РїРѕР»СѓС‡Р°РµРј СЃРїРёСЃРѕРє Р±РёР±Р»РёРѕС‚РµРє РґР»СЏ He=0 Рё С‡РёСЃР»Рѕ СЌС‚РёС… Р±РёР±Р»РёРѕС‚РµРє

		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf

		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd

		Print_process_percent(1000)
		Print "OK"




		For t = 0 To seans_num_out-1

			dat_all_str(h, t).ti_start = ti_loaded(t)-10
			dat_all_str(h, t).ti_end   = ti_loaded(t)+10

			dat_all_str(h, t).te_start = 500
			dat_all_str(h, t).te_end   = 4000

			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1

			dat_all_str(h, t).d_c = 1e200 ' очистка погрешностей

		Next t



		inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, 10) ' Step_Hyd = 2.5%, Step_Ti = 50K, Step_Te = 50K

		' 3 шаг
		ranges_set(h, Config_range_h_3, Config_range_te_3, 0)
		inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, 10) ' Step_Hyd = 1%, Step_Ti = 20K, Step_Te = 20K

		results_write2(h, he)

	Next he

	intervals_input_auto2(h)

End Sub


''' ================================================================



Sub inverse_problem_te(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer he, hyd
	Dim As Integer t
	Dim As Integer file
	Dim As Double  te_loaded(0 To seans_num_out-1)

	Cls()

	' Загрузка Te

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file

	For he = 0 To he_max Step he_step

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,

		' Загружаем библиотеки АКФ
		Print "Загрузка библиотек АКФ... ";


		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf

		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) ' РїРѕР»СѓС‡Р°РµРј СЃРїРёСЃРѕРє Р±РёР±Р»РёРѕС‚РµРє РґР»СЏ He=0 Рё С‡РёСЃР»Рѕ СЌС‚РёС… Р±РёР±Р»РёРѕС‚РµРє

		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf

		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd



		Print_process_percent(1000)
		Print "OK"






		For t = 0 To seans_num_out-1

			dat_all_str(h, t).te_start = te_loaded(t)-10
			dat_all_str(h, t).te_end   = te_loaded(t)+10

			dat_all_str(h, t).ti_start = 500
			dat_all_str(h, t).ti_end   = 4000

			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1

			dat_all_str(h, t).d_c = 1e200 ' очистка погрешностей

		Next t






		inverse_problem_v2_conv(h, z, Config_step_h_2, 10, Config_step_ti_2) ' Step_Hyd = 2.5%, Step_Ti = 50K, Step_Te = 50K

		' 3 шаг
		ranges_set(h, Config_range_h_3, 0, Config_range_ti_3)
		inverse_problem_v2_conv(h, z, Config_step_h_3, 10, Config_step_ti_3) ' Step_Hyd = 1%, Step_Ti = 20K, Step_Te = 20K


		results_write2(h, he)

	Next he

	intervals_input_auto2(h)

End Sub




''' ================================================================



Sub inverse_problem_hyd(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer he, hyd
	Dim As Integer t
	Dim As Integer file
	Dim As Double  hyd_loaded(0 To seans_num_out-1)

	Cls()

	' Загрузка Hyd
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd_loaded(t)
	Next t
	Close #file


	For he = 0 To he_max Step he_step

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,
		' Загружаем библиотеки АКФ

		Print "Загрузка библиотек АКФ... ";



		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf



		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) ' РїРѕР»СѓС‡Р°РµРј СЃРїРёСЃРѕРє Р±РёР±Р»РёРѕС‚РµРє РґР»СЏ He=0 Рё С‡РёСЃР»Рѕ СЌС‚РёС… Р±РёР±Р»РёРѕС‚РµРє

		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf

		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd


		Print_process_percent(1000)

		Print "OK"


		For t = 0 To seans_num_out-1

			dat_all_str(h, t).ti_start = 500
			dat_all_str(h, t).ti_end   = 4000

			dat_all_str(h, t).te_start = 500
			dat_all_str(h, t).te_end   = 4000

			dat_all_str(h, t).hyd_start = hyd_loaded(t)-1
			dat_all_str(h, t).hyd_end   = hyd_loaded(t)+1

			dat_all_str(h, t).d_c = 1e200 ' очистка погрешностей

		Next t



		inverse_problem_v2_conv(h, z, 1, Config_step_te_2, Config_step_ti_2) ' Step_Hyd = 2.5%, Step_Ti = 50K, Step_Te = 50K

		' 3 шаг
		ranges_set(h, 0, Config_range_te_3, Config_range_ti_3)
		inverse_problem_v2_conv(h, z, 1, Config_step_te_3, Config_step_ti_3) ' Step_Hyd = 1%, Step_Ti = 20K, Step_Te = 20K

		results_write2(h, he)

	Next he

	intervals_input_auto2(h)

End Sub


''' ================================================================


Sub inverse_problem_hyd_ti_te(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer he, hyd
	Dim As Integer t
	Dim As Integer file
	Dim As Double  hyd_loaded(0 To seans_num_out-1)
	Dim As Double  te_loaded(0 To seans_num_out-1)
	Dim As Double  ti_loaded(0 To seans_num_out-1)

	Cls()

	' Hyd
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd_loaded(t)
	Next t
	Close #file

	' Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file

	' Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file

	For he = 0 To he_max Step he_step

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,


		'
		Print "Загрузка библиотек АКФ... ";

		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf

		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) '


		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf


		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd

		Print_process_percent(1000)
		Print "OK"


		For t = 0 To seans_num_out-1

			dat_all_str(h, t).ti_start = ti_loaded(t)-10
			dat_all_str(h, t).ti_end   = ti_loaded(t)+10

			dat_all_str(h, t).te_start = te_loaded(t)-10
			dat_all_str(h, t).te_end   = te_loaded(t)+10

			dat_all_str(h, t).hyd_start = hyd_loaded(t)-1
			dat_all_str(h, t).hyd_end   = hyd_loaded(t)+1

			dat_all_str(h, t).d_c = 1e200 '

		Next t



		inverse_problem_v2_conv(h, z, 1, 10, 10) ' Step_Hyd = 0.5%, Step_Ti = 10K, Step_Te = 10K


		results_write2(h, he)

	Next he

	intervals_input_auto2(h)

End Sub




''' ================================================================



Sub inverse_problem_ti_te(ByVal h As Integer, ByVal z As Integer)

	Dim As Integer he, hyd
	Dim As Integer t
	Dim As Integer file
	Dim As Double  ti_loaded(0 To seans_num_out-1)
	Dim As Double  te_loaded(0 To seans_num_out-1)

	Cls()

	' Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file

	' Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file

	For he = 0 To he_max Step he_step

		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,

		' Загружаем библиотеки АКФ
		Print "Загрузка библиотек АКФ... ";

		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf


		libraries_num = fortran_library_list_get_conv_short_663(He, @libraries_filelist) ' РїРѕР»СѓС‡Р°РµРј СЃРїРёСЃРѕРє Р±РёР±Р»РёРѕС‚РµРє РґР»СЏ He=0 Рё С‡РёСЃР»Рѕ СЌС‚РёС… Р±РёР±Р»РёРѕС‚РµРє

		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf

		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			fortran_library_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd

		Print_process_percent(1000)
		Print "OK"




		For t = 0 To seans_num_out-1

			dat_all_str(h, t).ti_start = ti_loaded(t)-10
			dat_all_str(h, t).ti_end   = ti_loaded(t)+10

			dat_all_str(h, t).te_start = te_loaded(t)-10
			dat_all_str(h, t).te_end   = te_loaded(t)+10

			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1

			dat_all_str(h, t).d_c = 1e200 ' очистка погрешностей

		Next t



		inverse_problem_v2_conv(h, z, Config_step_h_2, 10, 10) ' Step_Hyd = 2.5%, Step_Ti = 50K, Step_Te = 50K

		' 3 шаг
		ranges_set(h, Config_range_h_3, 0, 0)
		inverse_problem_v2_conv(h, z, Config_step_h_3, 10, 10) ' Step_Hyd = 1%, Step_Ti = 20K, Step_Te = 20K

		results_write2(h, he)

	Next he

	intervals_input_auto2(h)

End Sub


''' ================================================================

Sub param_load(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)

	Dim As Integer t, file

	' загрузка временного хода Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti[t]
	Next t
	Close #file

	' загрузка временного хода Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te[t]
	Next t
	Close #file


	' загрузка временного хода H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd[t]
	Next t
	Close #file


	' загрузка временного хода He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he[t]
	Next t
	Close #file


	' загрузка временного хода D
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, d[t]
	Next t
	Close #file


End Sub



''' ================================================================

Sub param_load2(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)

	Dim As Integer t, file

	' загрузка временного хода Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti[t]
	Next t
	Close #file

	' загрузка временного хода Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te[t]
	Next t
	Close #file


	' загрузка временного хода H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd[t]
	Next t
	Close #file


	' загрузка временного хода He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he[t]
	Next t
	Close #file


	' загрузка временного хода D
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, d[t]
	Next t
	Close #file


End Sub



''' ================================================================

Sub param_save(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)

	Dim As Integer t, file

	' запись временного хода Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "####.# ";ti[t]
	Next t
	Close #file

	' запись временного хода Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "####.# ";te[t]
	Next t
	Close #file


	' запись временного хода H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "###.# "; hyd[t]
	Next t
	Close #file


	' запись временного хода He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "###.# "; he[t]
	Next t
	Close #file


	' запись временного хода D
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; d[t]
	Next t
	Close #file


End Sub


''' ================================================================

Sub equate_ti(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer)
	
	
	ReDim As Double ti_loaded(0 To seans_num_out-1)
	ReDim As Double te_loaded(0 To seans_num_out-1)

	Dim As Integer t
	Dim As Integer file

	' загрузка временного хода Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file

	' загрузка временного хода Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file


	For t = t_start To t_end ' по времени
	If ti_loaded(t) > te_loaded(t) Then
		ti_loaded(t) = te_loaded(t) 
	EndIf
	Next t

	' запись временного хода выбранного параметра
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file

	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; ti_loaded(t)
	Next t
	Close #file

End Sub


''' ================================================================

Sub draw_help()
	Dim key As Integer

	Cls
	Color 11
	Print "UPRISE version 1.1 beta"
	Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
	Print
	Color 7
	Print "Estimate - программа оценки параметров ионосферы (решение обратной задачи рассеяния)"
	Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
	Color 8
	Print
	Print "================================"
	Print "Программа собрана " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
	Print "================================"
	Color 15
	Print
	Print "Управление:"
	Print " Alt + Enter      Переключиться из оконного режима в полноэкранный и наоборот"
	Print "       Left/Right Перемещение по времени"
	Print "       Home       Перемещение курсора в начало экрана"
	Print "       End        Перемещение курсора в конец экрана"
	Print "       Tab        Перемещение курсора на 10 минут вправо"
	Print "Ctrl + Left/Right Сдвиг графиков по оси времени"
	Print "       Пробел     Начать/отменить выбор данных для интерполяции, пересчёта, ручной коррекции"
	Print "       F1         Вызов этой помощи"
	Print "Ctrl + N          Переход на следующую высоту"
	Print "       I          Интерполяция одного или всех параметров"
	Print "       R          Пересчёт параметров по одному интерполированному"
	Print "       P          Заполнение параметра указанным значением (ручная коррекция)"
	Print "       U          Отмена предыдущего действия (интерполяции, пересчёта, ручной коррекции)"
	Print "       T          Изменить ширину окна для расчёта тренда параметров на предыдущей высоте"
	Print
	Color 12
	Print "Нажмите Enter"

	Do
		key = GetKey
	Loop Until key = KEY_ENTER

	Color 15, 0
	Cls
End Sub

''' ================================================================

Sub input_param(ByVal h As Integer, ByVal z As Integer, ByVal t_start As Integer, ByVal t_end As Integer, ByVal param As Integer)

	ReDim As Double   d_loaded(0 To seans_num_out-1)
	ReDim As Double  ti_loaded(0 To seans_num_out-1)
	ReDim As Double  te_loaded(0 To seans_num_out-1)
	ReDim As Double hyd_loaded(0 To seans_num_out-1)
	ReDim As Double  he_loaded(0 To seans_num_out-1)

	ReDim As Double   d_he(0 To seans_num_out-1)
	ReDim As Double  ti_he(0 To seans_num_out-1)
	ReDim As Double  te_he(0 To seans_num_out-1)
	ReDim As Double hyd_he(0 To seans_num_out-1)

	Dim As Integer t, he
	Dim As Integer file

	Input "Введите значение He+: "; he

	' Загрузка Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_he(t)
	Next t
	Close #file

	' Загрузка Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_he(t)
	Next t
	Close #file

	' Загрузка H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd_he(t)
	Next t
	Close #file

	' Загрузка погрешностей
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, d_he(t)
	Next t
	Close #file


	param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )


	For t = t_start+1 To t_end-1 ' по времени
		he_loaded(t) = he
	Next t

	For t = t_start+1 To t_end-1 ' по времени
		d_loaded(t)   = d_he(t)
		ti_loaded(t)  = ti_he(t)
		te_loaded(t)  = te_he(t)
		hyd_loaded(t) = hyd_he(t)
	Next t

	param_save(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )

End Sub

''' ================================================================

Function getHeMax(h As Integer) As Integer

	Dim As Integer t
	Dim As Integer file
	ReDim As Double he(0 To seans_num_out-1)
	Dim As Double heMax

	' загрузка временного хода He+
	file = FreeFile()

	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(-1)+"."+Str(h)+".txt" For Input As #file

	If Err <> 0 Then
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(h)+".txt" For Input As #file
	EndIf


	For t = 0 To seans_num_out-1
		Input #file, he(t)
	Next t
	Close #file


	For t = 0 To seans_num_out-1
		heRange(t) = he(t)+He_grad
		If heRange(t) > He_maxLib Then
			heRange(t) = He_maxLib
		EndIf
	Next t


	heMax = heRange(0)
	For t = 1 To seans_num_out-1
		if heRange(t) > heMax Then
			heMax = heRange(t)
		EndIf
	Next t


	Return heMax

End Function

''' ================================================================
