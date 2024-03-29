
#Include Once "albom_lib.bi"	   ' �������� ���������� "albom.dll"
#Include Once "albom_log.bi"		' ����������� ����
#Include Once "albom_as_file.bi"	' �������� �������� � �������� ��� ������ � AS-�������
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "fbgfx.bi"			'����������� ����������� ����������

#Include "windows.bi"
#Include "file.bi"

#Include "window9.bi"
#Include "dir.bi"

#Include Once "sqlite3.bi"
#include Once "FBImage.bi"


'''==============================================

#If __FB_LANG__ = "fb"
	Using FB '��� �������� � ������������� ����� ��������
#EndIf

'''==============================================

' �������� ������ ��� �������������
Dim As Integer hZero = 60
ReDim Shared As Double Ambig(0 To 20, 0 To 50, 0 To hZero, 0 To 18)' partrap, tau, h, lag
ReDim Shared As Double AmbigCoeff(0 To 50, 0 To 1, 0 To 18)'tau, t, lag


Function coeff_load Cdecl (ByVal NotUsed As Any Ptr, _
ByVal argc As Integer, _
ByVal argv As ZString Ptr Ptr, _
ByVal colName As ZString Ptr Ptr) As Integer
	
	AmbigCoeff(CInt(*argv[0]), CInt(*argv[1]), CInt(*argv[2])) = CDbl(*argv[3])
	
	Return 0
	
End Function

'''==============================================

' ACF database

Dim Shared As sqlite3 Ptr db_acf

Dim Shared as STRING a_loaded
Dim Shared as Integer count_loaded

Function a_from_db Cdecl (ByVal NotUsed As Any Ptr, _
ByVal argc As Integer, _
ByVal argv As ZString Ptr Ptr, _
ByVal colName As ZString Ptr Ptr) As Integer
	a_loaded = *argv[0]
	
	Return 0
End Function


Function count_from_db Cdecl (ByVal NotUsed As Any Ptr, _
ByVal argc As Integer, _
ByVal argv As ZString Ptr Ptr, _
ByVal colName As ZString Ptr Ptr) As Integer
	count_loaded = CInt(*argv[0])
	
	Return 0
End Function


'''==============================================

Type dat_all_struct
	
	Dim	acf(0 To 18)	As Double
	Dim	q					As Double
	
	Dim	d_c				As Double
	Dim	ti_c				As Double	' ����������� �����
	Dim	te_c				As Double	' ����������� ����������
	Dim	hyd_c				As Double	' ������������� ���������� ����� ��������
	Dim	he_c				As Double	' ������������� ���������� ����� �����
	
	Dim	ti_start			As Double
	Dim	te_start			As Double
	Dim	hyd_start		As Double
	
	Dim	ti_end			As Double
	Dim	te_end			As Double
	Dim	hyd_end			As Double
	
	Dim	var(0 To 18)	As Double
	Dim	ratio				As Double
	
End Type

'''==============================================

Const As Double delta_tau = 30.555

'''==============================================

Dim Shared  As String LIBRARY_PATH
Dim Shared  As Integer pulse_length '663 ' ��� 795
Dim Shared  As Integer isConv
Dim Shared	As Integer num_point_acf ' 22 ��� 26, ����������� ������������� (pulse_length/delta_tau)

Dim Shared As Double DeltaTePlus   ' ������������ �������� ����������� ����������
Dim Shared As Double DeltaTeMinus  ' ������������ �������� ����������� ����������
Dim Shared As Double DeltaTiPlus   ' ������������ �������� ����������� �����
Dim Shared As Double DeltaTiMinus  ' ������������ �������� ����������� �����
Dim Shared As Double DeltaHydPlus  ' ������������ �������� Hyd (60 - ��� 30%)
Dim Shared As Double DeltaHydMinus ' ������������ �������� Hyd (60 - ��� 30%)

Dim Shared As Double  RegN
Dim Shared As Integer RegWnd			' ���� ��� ������� ���

Dim Shared As Integer  Config_step_h_1,  Config_step_ti_1,  Config_step_te_1
Dim Shared As Integer Config_range_h_2, Config_range_ti_2, Config_range_te_2
Dim Shared As Integer  Config_step_h_2,  Config_step_ti_2,  Config_step_te_2
Dim Shared As Integer Config_range_h_3, Config_range_ti_3, Config_range_te_3
Dim Shared As Integer  Config_step_h_3,  Config_step_ti_3,  Config_step_te_3
Dim Shared As Integer  Config_oxygen
Dim Shared As Integer  Config_auto
Dim Shared As Double  Config_coeff(0 To 18)
Dim Shared As Integer  Config_triangle
Dim Shared As Integer Config_ti_num
Dim Shared As Integer Config_te_num
Dim Shared As Integer Config_hyd_num

Dim Shared As Integer he_step

Dim Shared As Integer Config_sinus

Dim Shared As Integer ConfigAmbig

Dim Shared As Integer Config_h_min_q

Dim Shared As Integer isTrapVar

Dim Shared As Integer Config_hyd_not_decrease_hyd_plus_he

Dim Shared As Integer Config_Overlap_prohibition

Dim Shared As Integer Config_ti_interpolate, Config_ti_interpolate_num, Config_ti_interpolate_points, Config_ti_interpolate_dev

Dim Shared As Integer Config_wave

Dim Shared As Double Config_kappa

Dim Shared As Integer Config_sigma

Dim Shared As Integer Config_filter

Dim Shared As Double Config_alt_oxygen

'''==============================================

Dim Shared As Integer START_X = 0
Dim Shared As Integer CUR = 0

Dim Shared As Integer He_max
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


Dim As Integer nh'����� �����

Dim Shared As Integer Hmin, Hmax, Hstep

Dim As as_file_struct	as_file_in


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


ReDim Shared As Double RegRange(0 To 5, 0 To 1500) ' �������� ��� �������������

Dim As Integer He
Dim Shared As Integer heCurrent

Dim Shared As Double  acf_filter(0 To 100) ' ��� �������

Enum Params
	PARAM_TI
	PARAM_TE
	PARAM_H
	PARAM_HE
End Enum

Dim Shared As Integer trapProfile(0 To 679)

'''==============================================

Declare Sub inverse_problem_v1_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v2_conv(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v1(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v2(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v1_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v2_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
Declare Sub inverse_problem_v3_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer, ByVal he As Integer)

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
Declare Function getHydMax(h As Integer) As Integer

Declare Function analysis_param(ByVal h As Integer, ByVal z As Integer, ByVal param As Integer) As Integer

Declare Sub libraries_list_load(He As Integer)

Declare Sub save_and_exit()

Declare Sub interpolate_ti(ByVal h As Integer, ByVal z As Integer)

Declare Sub save_small_steps_results(ByVal h As Integer)


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
Print "Estimate - a program for estimating parameters of the ionosphere (inverse problem solving)"
Print "(c) O. Bogomaz, D. Kotov (Institute of Ionosphere, Kharkiv, Ukraine)"
Color 8
Print
Print "================================"
Print "Built " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
Print "================================"


Color 15


file = FreeFile()
Open "config/config.dat" For Input As #file
If Err() > 0 Then
	
	isTrapVar = 0
	
	Input "������� ������������ ������������ �������� (���): ", pulse_length
	LIBRARY_PATH = "d:/lib/"
	isConv = 0
	
	Input "Hmin: ", Hmin
	Input "Hmax: ", Hmax
	Input "Hstep: ", Hstep
	
	Input "He max �� ������ ������: ", He_max
	
	Input "|+deltaTe|: ",   DeltaTePlus   ' ������������ �������� ����������� ����������
	Input "|-deltaTe|: ",   DeltaTeMinus  ' ������������ �������� ����������� ����������
	Input "|+deltaTi|: ",   DeltaTiPlus   ' ������������ �������� ����������� �����
	Input "|-deltaTi|: ",   DeltaTiMinus  ' ������������ �������� ����������� �����
	Input "|+deltaHyd|: ",  DeltaHydPlus  ' ������������ �������� Hyd (60 - ��� 30%)
	Input "|-deltaHyd|: ",  DeltaHydMinus
	Input "N ���: ",   RegN
	Input "������ ���� ��� ������� ���: ",   RegWnd
	
	Config_step_h_1  = 10 : Config_step_ti_1  = 100 :  Config_step_te_1 = 100
	Config_range_h_2 = 20 : Config_range_ti_2 = 200 : Config_range_te_2 = 200
	Config_step_h_2  = 5  :  Config_step_ti_2 = 50  :  Config_step_te_2 = 50
	Config_range_h_3 = 10 : Config_range_ti_3 = 100 : Config_range_te_3 = 100
	Config_step_h_3  = 2  : Config_step_ti_3  = 20  : Config_step_te_3  = 20
	
	Config_oxygen = 0
	
	he_step = 1
	
	Config_sinus = 1
	
	Config_auto = 1
	
	For i = 0 To 18
		Config_coeff(i) = 1
	Next i
	
	Config_triangle = 1
	
	ConfigAmbig = 0
	
	Config_h_min_q = 45
	
	Config_filter = 9
	
	Config_alt_oxygen = 0
	
Else
	
	Dim As String tmp_string
	
	' 1
	Input #file, tmp_string ' � ������ ������ �������� ��� ����� �������������� ����������, � ������ ��������� ��� �� ������������
	
	' 2
	Input #file, isTrapVar
	
	' 3
	Input #file, isConv
	
	' 4
	Input #file, LIBRARY_PATH
	
	' 5
	Input #file, pulse_length
	
	' 6
	Input #file, Hmin
	
	' 7
	Input #file, Hmax
	
	' 8
	Input #file, Hstep
	
	' 9
	Input #file, He_max
	
	' 10
	Input #file, DeltaTePlus, DeltaTeMinus   ' ������������ �������� ����������� ����������
	
	' 11
	Input #file, DeltaTiPlus, DeltaTiMinus   ' ������������ �������� ����������� �����
	
	' 12
	Input #file, DeltaHydPlus, DeltaHydMinus  ' ������������ �������� Hyd (60 - ��� 30%)
	
	' 13
	Input #file, RegN
	
	' 14
	Input #file, RegWnd
	
	' 15
	Input #file, Config_step_h_1,  Config_step_ti_1,  Config_step_te_1
	
	' 16
	Input #file, Config_range_h_2, Config_range_ti_2, Config_range_te_2
	
	' 17
	Input #file, Config_step_h_2,  Config_step_ti_2,  Config_step_te_2
	
	' 18
	Input #file, Config_range_h_3, Config_range_ti_3, Config_range_te_3
	
	' 19
	Input #file, Config_step_h_3,  Config_step_ti_3,  Config_step_te_3
	
	' 20
	Input #file, he_step
	
	' 21
	Input #file, Config_oxygen
	
	' 22
	Input #file, Config_sinus
	
	' 23
	Input #file, He_grad
	
	' 24
	Input #file, He_maxLib
	
	' 25
	Input #file, Config_auto
	
	' 26
	For i = 0 To 18
		Input #file, Config_coeff(i)
	Next i
	
	' 27
	Input #file, Config_triangle
	
	' 28
	Input #file, Config_ti_num
	
	' 29
	Input #file, Config_te_num
	
	' 30
	Input #file, Config_hyd_num
	
	' 31
	Input #file, ConfigAmbig
	
	' 32
	Input #file, Config_h_min_q
	
	' 33
	Input #file, Config_hyd_not_decrease_hyd_plus_he
	
	' 34
	Input #file, Config_Overlap_prohibition
	
	' 35
	Input #file, Config_ti_interpolate, Config_ti_interpolate_num, Config_ti_interpolate_points, Config_ti_interpolate_dev
	
	'36
	Input #file, Config_wave
	
	'37
	Input #file, Config_kappa
	
	'38
	Input #file, Config_sigma
	
	'39
	Input #file, Config_filter
	
	'40
	Input #file, Config_alt_oxygen
	
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
Print "��������� ��������� ���������. ��� ������������� ������� ����������� � config.dat � ������������� ���������."
Print
Color 15
Print "������������ ������������ ��������: "; pulse_length; " ���. ";
If Config_triangle <> 0 Then
	Print "���������� �������������� ������ �����������. "
Else
	Print "���������� �������������� ������ �� �����������. "
EndIf

If ConfigAmbig <> 0 Then
	Print "������ ���������� � �������������� ���. ";
Else
	Print "������ ���������� ��� ������������� ���. ";
EndIf


Print "���� � ����������� ���: "; LIBRARY_PATH
Print
Print "����� ����������� ������ Hmin: "; Hmin
Print "����� ������������ ������ Hmax: "; Hmax
Print "��� �� ������ Hstep: "; Hstep

If Config_oxygen = 0 Then
	Print
	Print "He max �� ������ ������: ";  He_max; " %"; "       He max: "; He_maxLib; " %"
EndIf

Print
Print "��������� �� Te (�� 4.5 ��): +"; DeltaTePlus; " K, -"; DeltaTeMinus; " K"
Print "��������� �� Ti (�� 4.5 ��): +"; DeltaTiPlus; " K, -"; DeltaTiMinus; " K"

If Config_oxygen = 0 Then
	Print "��������� �� H+ (�� 4.5 ��): +"; DeltaHydPlus; ", -"; DeltaHydMinus, "("; DeltaHydPlus/2; " %, -"; DeltaHydMinus/2; " % )"
EndIf


Print "���������� ���: "; RegN
Print "������ ���� ��� ������� ���: "; RegWnd

Print

If Config_oxygen = 0 Then
	
	Print "���� ��� 1-� ����������� (H+, Ti, Te):      ";  Config_step_h_1; " ("; Config_step_h_1/2;" % ), " ; Config_step_ti_1; " K, "; Config_step_te_1; " K"
	Print
	Print "��������� ��� 2-� ����������� (H+, Ti, Te):   +-";  Config_range_h_2; " (+-"; Config_range_h_2/2;" % ), +-" ; Config_range_ti_2; " K, +-"; Config_range_te_2; " K"
	Print "���� ��� 2-� ����������� (H+, Ti, Te):      ";  Config_step_h_2; " ("; Config_step_h_2/2;" % ), " ; Config_step_ti_2; " K, "; Config_step_te_2; " K"
	Print
	Print "��������� ��� 3-� ����������� (H+, Ti, Te):   +-";  Config_range_h_3; " (+-"; Config_range_h_3/2;" % ), +-" ; Config_range_ti_3; " K, +-"; Config_range_te_3; " K"
	Print "���� ��� 3-� ����������� (H+, Ti, Te):      ";  Config_step_h_3; " ("; Config_step_h_3/2;" % ), " ; Config_step_ti_3; " K, "; Config_step_te_3; " K"
	Print
	Print "��� �� He+: ", he_step; "%"
	Print "�������� �� He+ (�� 4.5 ��): "; he_grad; "%"
Else
	Print "���� ��� 1-� ����������� (Ti, Te):      ";   Config_step_ti_1; " K, "; Config_step_te_1; " K"
	Print
	Print "��������� ��� 2-� ����������� (Ti, Te):   +-";  Config_range_ti_2; " K, +-"; Config_range_te_2; " K"
	Print "���� ��� 2-� ����������� (Ti, Te):      ";  Config_step_ti_2; " K, "; Config_step_te_2; " K"
	Print
	Print "��������� ��� 3-� ����������� (Ti, Te):   +-";  Config_range_ti_3; " K, +-"; Config_range_te_3; " K"
	Print "���� ��� 3-� ����������� (Ti, Te):      "; Config_step_ti_3; " K, "; Config_step_te_3; " K"
EndIf

If Config_oxygen <> 0 Then
	Color 12
	Print
	Print "��������! ������������ ����������� �����������."
EndIf

Color 15
Print
If Config_sinus <> 0 Then
	Print "�������� ������������ �����������."
Else
	Print "�������� ������������ �� �����������."
EndIf

If Config_auto <> 0 Then
	Print
	Print "������� �������������� �����."
EndIf


Print
Print "������� ������������: ";
For i = 0 To 18
	Print Using "##.# "; Config_coeff(i);
Next i

Print
Print
Print "���������� ����� ��� ����������� ��������� ����� (�� Ti, Te � H+): "; Config_ti_num; " "; Config_te_num; " "; Config_hyd_num

Print
Color 12
Print "������� Enter"

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

If Config_Overlap_prohibition <> 0 Then
	Config_Overlap_prohibition = 1
EndIf

If Config_ti_interpolate <> 0 Then
	Config_ti_interpolate = 1
EndIf



Cls()
Color 11

Print "���������� ���������, ����������� � ����� " + Chr(34) + "out" + Chr(34) + ":"
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

num_point_acf = CDbl(pulse_length)/delta_tau

Print
Input "������� ���� ������ ��������� (���� ����� ���): ", d_day, d_month, d_year
Input "������� ���������� �����: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

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
Print "������� ���������� ������� ������... ";


seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"



seans_num_out = seans_num_in

DeleteDir(SEANS_DIR_OUT + DirectoryOutput+"/step3", /'FOF_ALLOWUNDO Or '/FOF_NOCONFIRMATION Or FOF_SILENT)
MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")

FileCopy("config/config.dat", SEANS_DIR_OUT + DirectoryOutput+"/step3/config.dat")

If isConv = 0 Then
	
	' �������� ��� �� �������
	file = FreeFile()
	Open "hardware/filter.dat" For Input As #file
	If Err <> 0 Then
		PrintErrorToLog(ErrorFilter, __FILE__, __LINE__)
		End
	EndIf
	For tau = 0 To num_point_acf-1
		Input #file, acf_filter(tau)
	Next tau
	Close #file
	
EndIf




If as_file_load(SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+".0001", @as_file_in) = 0 Then ' ��������� ���� (��� ���� ���������� ������, ������� � ���������� ����� �����������)
	PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
EndIf

nh = as_file_in.nh ' �������� ����� �����


ReDim Shared As Integer Hkm(0 To nh-1)

For h = 0 To nh-1
	Hkm(h) = as_file_in.acf[h].h
Next h

as_file_close( @as_file_in ) ' ����������� ������, ���������� �� �����




' ������ ���� ��� �������� �����
file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Output As #file
Close #file




' ��������� ������ ���������� (��� ������� ���������� ����������)
temperatures_len = library_light_list_of_temperatures_get(@temperatures(0))



' �������� ������ ��� ������
Print "��������� ������ ��� ������... ";
ReDim Shared As dat_all_struct dat_all_str(0 To nh-1, 0 To seans_num_out-1)
If Err() <> 0 Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf



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

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"HEADER.txt" For Output As #file
Close #file

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"Fkr.txt" For Output As #file
Close #file

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"Pn.txt" For Output As #file
Close #file


' ��������� ��� � ���
Print "�������� ������... ";
For t = 0 To seans_num_out-1 ' �� �������
	Print_process_percent((t*100)/seans_num_out)
	
	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext
	
	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext
	
	If as_file_load(filename, @as_file_in) = 0 Then ' ��������� ����� (��� ���� ���������� ������, ������� � ���������� ����� �����������)
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
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"HEADER.txt" For Append As #file
	Dim As String tmp
	If CInt( Mid(as_file_in.date_, 7, 2) ) > 90 Then
		tmp = "19"+Mid(as_file_in.date_, 7, 2)
	Else
		tmp = "20"+Mid(as_file_in.date_, 7, 2)
	EndIf
	Print #file, Using "&-&-&     &      N= &      T= &"; Mid(as_file_in.date_, 1, 2); Mid(as_file_in.date_, 4, 2); tmp; as_file_in.time_; as_file_in.nseans; as_file_in.tnak
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"Pn.txt" For Append As #file
	Print #file, Using "&"; as_file_in.rnc(0)
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
		
		dat_all_str(h, t).d_c = 1e200
		
		dat_all_str(h, t).q = as_file_in.acf[h].q
		
		For tau = 0 To 18
			dat_all_str(h, t).var(tau) = as_file_in.acf[h].var(tau)
		Next tau
		
	Next h
	
	as_file_close(@as_file_in)
	
Next t
Print_process_percent(1000)
Print "OK"

Print #1, Str(seans_num_out)+" files loaded"
Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"


file = FreeFile()
Open SEANS_DIR_OUT+DirectoryOutput+"/step2/profileTrap.txt" For Input As #file

For h = 0 To 679
	Dim As Double tmp
	Input #file, tmp, trapProfile(h)
Next h

Close #file





' �������� ���
Print "�������� ���... ";
For partrap As Integer = 0 To 0'20
	
	Print_process_percent((lag*100)/21)
	
	Dim As String DirNum
	
	If partrap < 10 Then
		DirNum = "0" + Str(partrap)
	Else
		DirNum = Str(partrap)
	EndIf
	
	
	For lag = 0 To 18
		file = FreeFile()
		ext = Str(lag)
		If lag < 10 Then ext = "0"+ext
		If pulse_length = 663 Then
			filename = "./ambig/663-"+Str(Config_filter)+"/"+ DirNum +"/ambig"+ext+".dat"
		Else
			If pulse_length = 795 Then
				filename = "./ambig/795-"+Str(Config_filter)+"/"+"00/ambig"+ext+".dat"
			EndIf
		EndIf
		
		Print #1, filename
		Open filename For Input As #file
		If Err() <> 0 Then
			PrintErrorToLog(ErrorAFunction, __FILE__, __LINE__)
			End
		EndIf
		For h = 0 To hZero
			For tau = 0 To 50
				Input #file, Ambig(partrap, tau, h, lag)
			Next tau
		Next h
		Close #file
	Next lag
Next partrap
Print_process_percent(1000)
Print "OK"


Print "������ �������������... ";

ReDim Shared As Double AmbigCoeff(0 To 50, 0 To seans_num_out-1, 0 To 18)'tau, t, lag

Dim Shared As sqlite3 Ptr db
Dim Shared As ZString Ptr errMsg

Dim Shared As String database_name
database_name = SEANS_DIR_OUT + DirectoryOutput + "/step3/" + "AmbigCoeff.db"
Dim As String create_query = "CREATE TABLE IF NOT EXISTS coeff(tau INT, h INT, t INT, lag INT, val DOUBLE);"
Dim As String begin_query = "BEGIN;"
Dim As String commit_query = "COMMIT;"
Dim As String insert_query_b = "INSERT INTO coeff (tau, h, lag, t, val) VALUES ("
Dim Shared As String insert_query

Kill(database_name)
If sqlite3_open(database_name, @db) <> SQLITE_OK Then
	Print "Can't open database: "; *sqlite3_errmsg(db)
	sqlite3_close(db)
	break
End If

If sqlite3_exec(db, create_query, 0, 0,  @errMsg) <> SQLITE_OK Then
	Print "SQL error: "; *errMsg
	break
EndIf

For t = 0 To seans_num_out-1
	
	Print_process_percent((t*100)/seans_num_out)
	
	If sqlite3_exec(db, begin_query, 0, 0,  @errMsg) <> SQLITE_OK Then
		Print "SQL error: "; *errMsg
		break
	EndIf
	
	For h = hMin To hMax Step hStep
		For lag = 0 To 18
			For tau = 0 To 50
				AmbigCoeff(tau, t, lag) = 0
				For z = 0 To hZero+trapProfile(h)*2
					If h-z+num_point_acf\2+trapProfile(h) > 0 Then
						AmbigCoeff(tau, t, lag) += dat_all_str(h-z+num_point_acf\2+trapProfile(h), t).acf(0) * Ambig(trapProfile(h), tau, z, lag)
					EndIf
				Next z
				
				insert_query = insert_query_b + Str(tau) + ", " + Str(h) + ", " + Str(lag) + ", " + Str(t) + ", " + Str(AmbigCoeff(tau, t, lag)) + ");"
				If sqlite3_exec(db, insert_query, 0, 0,  @errMsg) <> SQLITE_OK Then
					Print "SQL error: "; *errMsg
					break
				EndIf
				
			Next tau
		Next lag
	Next h
	
	If sqlite3_exec(db, commit_query, 0, 0,  @errMsg) <> SQLITE_OK Then
		Print "SQL error: "; *errMsg
		break
	EndIf
	
	
Next t


Print_process_percent(1000)
Print "OK"


''' ACF database

Dim Shared As String database_acf_name
database_acf_name = SEANS_DIR_OUT + DirectoryOutput + "/step3/" + "ACFs.db"
Dim Shared As String create_acf_query
create_acf_query = "CREATE TABLE IF NOT EXISTS acf(ti INT, te INT, a STR);"
Dim Shared As String delete_data_query
delete_data_query = "DELETE FROM acf;"
Dim Shared As String insert_query_acf_b
insert_query_acf_b = "INSERT INTO acf (ti, te, a) VALUES ("
Dim Shared As String count_query_acf_b
count_query_acf_b = "SELECT COUNT (*) FROM acf WHERE "
Dim Shared As String count_query
Dim Shared As String select_query_acf_b
select_query_acf_b = "SELECT a FROM acf WHERE "
Dim Shared As String select_query

If sqlite3_open(database_acf_name, @db_acf) <> SQLITE_OK Then
	Print "Can't open database: "; *sqlite3_errmsg(db_acf)
	PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
	sqlite3_close(db_acf)
	sqlite3_close(db)
	break
End If


''' CREATE TABLE

If sqlite3_exec(db_acf, create_acf_query, 0, 0,  @errMsg) <> SQLITE_OK Then
	Print "SQL error: "; *errMsg
	PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
	break
EndIf

' ������� �������� ������

If Config_oxygen <> 0 Then
	he_max = 0
EndIf


z = 0
For h = Hmin To Hmax Step Hstep ' �� ������
	
	Cls
	Print "������� �������� ������... "
	Print #1, "Inverse problem solving... h="+Str(CInt(Hkm(h)))+" km"
	
	Dim As String select_query_b = "SELECT tau, t, lag, val FROM coeff WHERE h="
	Dim As String select_query = select_query_b + Str(h) + ";"
	
	If sqlite3_exec(db, select_query, @coeff_load, 0,  @errMsg) <> SQLITE_OK Then
		Print "SQL error: "; *errMsg
		break
	EndIf
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Q."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1 ' �� �������
		Print #file, dat_all_str(h, t).q
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "P."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1 ' �� �������
		Print #file, dat_all_str(h, t).acf(0)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	Close #file
	
	If Config_auto <> 0 Then
		Print
		Color 12
		Print "�������������� �����."
		Print "��� �������� � ������ ����� ������� ESC."
		Print
		Color 15
	EndIf
	
	Color 11
	Print "��� ��������� ��������� � ���������� ����������� ������� Q."
	Print
	Color 15
	
	Dim As Integer auto
	
	auto = 1
	
	If Hkm(h) < Config_alt_oxygen Then
		he_max = 0
	EndIf
	
	For he = 0 To he_max Step he_step
		
		If Config_auto <> 0 Then
			If MultiKey(FB.SC_ESCAPE) And auto = 1 Then
				auto = 0
				Color 11
				Print "������� ������ �����."
				Color 15
			EndIf
		EndIf
		
		If MultiKey(FB.SC_Q) Then
			Color 10
			Print
			Print "��������� ���������� � ����� �� ���������? (Y/N) "
			If GetKey_YN() <> 0 Then
				save_and_exit()
				End
			EndIf
			Color 15
		EndIf
		
		heCurrent = he
		
		Dim As Double qMin = 1e200
		For t = 0 To seans_num_out-1 ' �� �������
			if dat_all_str(h, t).q < qMin Then
				qMin = dat_all_str(h, t).q
			EndIf
		Next t
		
		Dim As Double qMax = -1e200
		For t = 0 To seans_num_out-1 ' �� �������
			If dat_all_str(h, t).q > qMax Then
				qMax = dat_all_str(h, t).q
			EndIf
		Next t
		
		Print Using "n = ###   h = ####   He+ = ##   Qmin = ####.##   Qmax = ####.##   "; h; Hkm(h); he; qMin; qMax;
		Print ,
		
		' ����� � ���� �������� �����
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(CInt(Hkm(h)))+".txt" For Append As #file
		Print #file, He
		Close #file
		
		' ��������� ���������� ���
		Print "�������� ��������� ���... ";
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		libraries_list_load(He)
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		If Hkm(h) < Config_alt_oxygen Then
			libraries_num = 1
		EndIf
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
			zfilename = LIBRARY_PATH + zfilename
			libraries_file(hyd) = fopen (zfilename, "rb")
			If libraries_file(hyd) = NULL Then
				PrintErrorToLog(ErrorFortranLib, __FILE__, __LINE__)
				End
			EndIf
		Next hyd
		
		Print_process_percent(1000)
		Print "OK"
		
		
		
		
		' ������� ������������
		For t = 0 To seans_num_out-1
			dat_all_str(h, t).d_c = 1e200
		Next t
		
		ranges_reset(h)
		
		If Config_auto <> 0 Then
			If MultiKey(FB.SC_ESCAPE) And auto = 1 Then
				auto = 0
				Color 11
				Print "������� ������ �����."
				Color 15
			EndIf
		EndIf
		
		
		
		' 1 ���
		If isConv <> 0 Then
			inverse_problem_v1_conv(h, z, Config_step_h_1, Config_step_te_1, Config_step_ti_1)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v1_ambig(h, z, Config_step_h_1, Config_step_te_1, Config_step_ti_1)
			Else
				inverse_problem_v1(h, z, Config_step_h_1, Config_step_te_1, Config_step_ti_1)
			EndIf
		EndIf
		
		
		
		If Config_auto <> 0 Then
			If MultiKey(FB.SC_ESCAPE) And auto = 1 Then
				auto = 0
				Color 11
				Print "������� ������ �����."
				Color 15
			EndIf
		EndIf
		
		' 2 ���
		ranges_set(h, Config_range_h_2, Config_range_te_2, Config_range_ti_2)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			Else
				inverse_problem_v2(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			EndIf
		EndIf
		
		
		If Config_auto <> 0 Then
			If MultiKey(FB.SC_ESCAPE) And auto = 1 Then
				auto = 0
				Color 11
				Print "������� ������ �����."
				Color 15
			EndIf
		EndIf
		
		
		' 3 ���
		ranges_set(h, Config_range_h_3, Config_range_te_3, Config_range_ti_3)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		results_write(h, he)
		
		ranges_set(h, 2, 20, 20)
		inverse_problem_v3_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3, he)
		
		If Config_auto <> 0 Then
			If MultiKey(FB.SC_ESCAPE) And auto = 1 Then
				auto = 0
				Color 11
				Print "������� ������ �����."
				Color 15
			EndIf
		EndIf
		
	Next he
	
	intervals_input_auto(h)
	save_small_steps_results(h)
	
	If (auto = 0) Or (Config_auto = 0) Then
		
		draw_all(h, z)
		
	Else
		
		'Dim As Integer wnd = 1
		
		save(h)
		
		If Config_ti_interpolate = 1 Then
			interpolate_ti(h, z)
		EndIf
		
		trand_ti (h, z, 1, seans_num_out-1, Config_ti_num)
		trand_te (h, z, 1, seans_num_out-1, Config_te_num)
		inverse_problem_ti_te(h, z)
		
	EndIf
	
	
	
	
	' ������ �������� ������������ ������ � ����
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+"H.txt" For Append As #file
	Print #file, Str(CInt(Hkm(h)))
	Close #file
	
	
	ranges_reg_set(h) ' �������������
	
	z += 1
	
	He_max = getHeMax( CInt(Hkm(h)) ) ' He_max2
	
	If Config_oxygen <> 0 Then
		he_max = 0
	EndIf
	
	If Hkm(h) < Config_alt_oxygen Then
		he_max = 0
	EndIf
	
Next h ' !!!!


save_and_exit()

Print
Print "OK"
break

'''==============================================




Sub inverse_problem_v1_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
	
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
				
				If (te >= ti And Config_Overlap_prohibition = 1) Or (Config_Overlap_prohibition = 0) Then
					
					If (te/ti <= 4) And (te/ti >= 0.7) Then
						
						If acf_library_light_short( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_lib(25), num_point_acf) <> 0 Then
							
							For tau = 0 To 24
								acf_lib(tau) = acf_lib(50-tau)
							Next tau
							
							For t = 0 To seans_num_out-1 ' �� �������
								
								
								If ( te >= dat_all_str(h, t).te_start ) And ( te <= dat_all_str(h, t).te_end ) And ( ti >= dat_all_str(h, t).ti_start ) And ( ti <= dat_all_str(h, t).ti_end ) And ( hyd >= dat_all_str(h, t).hyd_start ) And ( hyd <= dat_all_str(h, t).hyd_end ) Then
									
									If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) And ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) And ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
										
										For lag = 0 To 18
											acf_teor(lag) = 0
											For tau = 0 To 50
												acf_teor(lag) += acf_lib(tau) * AmbigCoeff(tau, t, lag)
											Next tau
										Next lag
										
										'!									For lag = 0 To 18
										'!										acf_teor(lag) /= lag+1
										'!									Next lag
										
										d = 0
										If Config_sigma <> 0 Then
											For tau = 1 To 18
												If dat_all_str(h, t).var(tau) <> 0 Then '!!! ������� ��� (���� ��������� �� �����-�� �������� ��������� ����� 0)
													d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2 / dat_all_str(h, t).var(tau)
												Else
													d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
												EndIf
												
											Next tau
										Else
											For tau = 1 To 18
												d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
											Next tau
										EndIf
										
										Dim As Double ratio = 0
										
										If z < 2 Then
											dat_all_str(h, t).ratio = 0
										Else
											Dim As Double chi2constraint = (te - 2*dat_all_str(h-Hstep, t).te_c + dat_all_str(h-2*Hstep, t).te_c)^2 + _
											(ti - 2*dat_all_str(h-Hstep, t).ti_c + dat_all_str(h-2*Hstep, t).ti_c)^2
											If chi2constraint <> 0 Then
												ratio = chi2constraint/d
												d += Config_kappa*ratio
											Else
												ratio = 0
											EndIf
										EndIf
										
										If d < dat_all_str(h, t).d_c Then
											dat_all_str(h, t).d_c = d
											dat_all_str(h, t).ti_c = ti
											dat_all_str(h, t).te_c = te
											dat_all_str(h, t).hyd_c = hyd
											dat_all_str(h, t).ratio = ratio
										EndIf
										
										'EndIf
										
									EndIf
									
								EndIf
								
								
							Next t
							
						EndIf
						
					EndIf
					
				EndIf
				
				
				
			Next ti
			
		Next te
		
	Next hyd
	
	
End Sub

''' ================================================================






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
				
				If (te/ti <= 3.5) And (te/ti >= 0.7) Then
					
					If acf_library_light_short_conv( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), 19) <> 0 Then
						
						For t = 0 To seans_num_out-1 ' �� �������
							
							If ( te >= dat_all_str(h, t).te_start ) And ( te <= dat_all_str(h, t).te_end ) And ( ti >= dat_all_str(h, t).ti_start ) And ( ti <= dat_all_str(h, t).ti_end ) And ( hyd >= dat_all_str(h, t).hyd_start ) And ( hyd <= dat_all_str(h, t).hyd_end ) Then
								
								If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) And ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) And ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
									
									d = 0
									For tau = 1 To 18
										d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
									Next tau
									
									If d < dat_all_str(h, t).d_c Then
										dat_all_str(h, t).d_c = d
										dat_all_str(h, t).ti_c = ti
										dat_all_str(h, t).te_c = te
										dat_all_str(h, t).hyd_c = hyd
									EndIf
									
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



Sub inverse_problem_v1(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
	
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
				
				If (te/ti <= 3.5) And (te/ti >= 0.7) Then
					
					If acf_library_light_short( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), num_point_acf) <> 0 Then
						
						If Config_triangle <> 0 Then
							For tau = 0 To num_point_acf-1
								acf_teor(tau) *= 1-tau*delta_tau/pulse_length
							Next tau
						EndIf
						
						func_conv_d(@acf_teor(0), @acf_filter(0), @acf_teor(0), num_point_acf)
						array_norm0_d(@acf_teor(0), @acf_teor(0), num_point_acf)
						
						For t = 0 To seans_num_out-1 ' �� �������
							
							If ( te >= dat_all_str(h, t).te_start ) And ( te <= dat_all_str(h, t).te_end ) And ( ti >= dat_all_str(h, t).ti_start ) And ( ti <= dat_all_str(h, t).ti_end ) And ( hyd >= dat_all_str(h, t).hyd_start ) And ( hyd <= dat_all_str(h, t).hyd_end ) Then
								
								If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) And ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) And ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
									
									If heCurrent <= heRange(t) Then
										
										d = 0
										For tau = 1 To 18
											d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
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
							
						Next t
						
					EndIf
					
				EndIf
				
			Next ti
			
		Next te
		
	Next hyd
	
	
End Sub

''' ================================================================



Sub inverse_problem_v2_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
	
	Dim As Integer hyd
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d
	
	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)
	
	For hyd = 0 To libraries_num-1 Step step_hyd
		
		For t = 0 To seans_num_out-1 ' �� �������
			
			If ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
				
				If (hyd >= dat_all_str(h, t).hyd_start) And (hyd <= dat_all_str(h, t).hyd_end) Then
					
					For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te
						
						If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) Then
							
							For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti
								
								If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) Then
									
									If (te >= ti And Config_Overlap_prohibition = 1) Or (Config_Overlap_prohibition = 0) Then
										If (te/ti <= 4) And (te/ti >= 0.7) Then
											
											If acf_library_light_short( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_lib(25), num_point_acf) <> 0 Then
												
												For tau = 0 To 24
													acf_lib(tau) = acf_lib(50-tau)
												Next tau
												
												For lag = 0 To 18
													acf_teor(lag) = 0
													For tau = 0 To 50
														acf_teor(lag) += acf_lib(tau) * AmbigCoeff(tau, t, lag)
													Next tau
												Next lag
												
												d = 0
												If Config_sigma <> 0 Then
													For tau = 1 To 18
														If dat_all_str(h, t).var(tau) <> 0 Then
															d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2 / dat_all_str(h, t).var(tau)
														Else
															d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
														EndIf
														
													Next tau
												Else
													For tau = 1 To 18
														d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
													Next tau
												EndIf
												
												Dim As Double ratio = 0
												
												If z < 2 Then
													dat_all_str(h, t).ratio = 0
												Else
													Dim As Double chi2constraint = (te - 2*dat_all_str(h-Hstep, t).te_c + dat_all_str(h-2*Hstep, t).te_c)^2 + _
													(ti - 2*dat_all_str(h-Hstep, t).ti_c + dat_all_str(h-2*Hstep, t).ti_c)^2
													If chi2constraint <> 0 Then
														ratio = chi2constraint/d
														d += Config_kappa*ratio
													Else
														ratio = 0
													EndIf
												EndIf
												
												If d < dat_all_str(h, t).d_c Then
													dat_all_str(h, t).d_c = d
													dat_all_str(h, t).ti_c = ti
													dat_all_str(h, t).te_c = te
													dat_all_str(h, t).hyd_c = hyd
													dat_all_str(h, t).ratio = ratio
												EndIf
												
											EndIf
											
										EndIf
										
									EndIf
									
								EndIf
								
							Next ti
							
						EndIf
						
					Next te
					
				EndIf
				
			EndIf
			
		Next t
		
	Next hyd
	
End Sub


''' ================================================================



Sub inverse_problem_v3_ambig(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer, ByVal he As Integer)
	
	Dim As Double hyd
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d
	
	ReDim As Double  d_c(0 To seans_num_out-1)
	ReDim As Double  ti_c(0 To seans_num_out-1)
	ReDim As Double te_c (0 To seans_num_out-1)
	ReDim As Double hyd_c(0 To seans_num_out-1)
	ReDim As Double ratio_c(0 To seans_num_out-1)
	
	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)
	
	if z = 0 then
		
		For t = 0 To seans_num_out-1
			d_c(t) = dat_all_str(h, t).d_c
			ti_c(t) = dat_all_str(h, t).ti_c
			te_c(t) = dat_all_str(h, t).te_c
			hyd_c(t) = dat_all_str(h, t).hyd_c / 2
			ratio_c(t) = dat_all_str(h, t).ratio
		Next

	else
		
		For t = 0 To seans_num_out-1
			d_c(t) = 1e200
		Next
		
		For hyd = 0 To 100 Step 0.1
			
			''' DELETE DATA FROM TABLE
			If sqlite3_exec(db_acf, delete_data_query, 0, 0, @errMsg) <> SQLITE_OK Then
				Print "SQL error: "; *errMsg
				PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
				break
			EndIf
			
			For t = 0 To seans_num_out-1 ' �� �������
				
				If ( hyd >= RegRange(4, t)/2 ) And ( hyd <= RegRange(5, t)/2 ) Then
					
					If (hyd >= dat_all_str(h, t).hyd_start/2) And (hyd <= dat_all_str(h, t).hyd_end/2) Then
						
						For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te
							
							If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) and (te >= 500) and (te <= 4000) ) Then
								
								For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti
									
									If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) and (ti >= 500) and (ti <= 4000) ) Then
										
										If (te >= ti And Config_Overlap_prohibition = 1) Or (Config_Overlap_prohibition = 0) Then
											If (te/ti <= 4) And (te/ti >= 0.7) Then

												''' SELECT COUNT
												count_query = count_query_acf_b + " ti=" + Str(ti) + " AND te=" + Str(te) + ";"
												If sqlite3_exec(db_acf, count_query, @count_from_db, 0, @errMsg) <> SQLITE_OK Then
													Print "SQL error: "; *errMsg
													PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
													break
												EndIf

												Dim as DOUBLE acf_current(0 to 24)
												if count_loaded = 0 then

													acf_3_kharkiv_22(hyd/100.0, he/100.0, ti, te, @acf_current(0))

													Dim As String a_encoded
													a_encoded = base64.EncodeMemory(@acf_current(0), 25*Sizeof(Double))												

													''' INSERT												
													insert_query = insert_query_acf_b + Str(ti) + ", " + Str(te) + ", '" + a_encoded + "');"
													If sqlite3_exec(db_acf, insert_query, 0, 0, @errMsg) <> SQLITE_OK Then
														Print "SQL error: "; *errMsg
														PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
														break
													EndIf

												else

													''' SELECT
													select_query = select_query_acf_b + "ti=" + Str(ti) + " AND te=" + Str(te) + ";"
													If sqlite3_exec(db_acf, select_query, @a_from_db, 0, @errMsg) <> SQLITE_OK Then
														Print "SQL error: "; *errMsg
														PrintErrorToLog(ErrorDB, __FILE__, __LINE__)
														break
													EndIf

													Dim As any ptr decoded_ptr
													decoded_ptr = base64.DecodeMemory(a_loaded, 25*Sizeof(Double))
													memcpy(@acf_current(0), decoded_ptr, 25*sizeof(Double))

												EndIf

												' print hyd, te, ti

												For tau = 0 To 24
													acf_lib(tau+25) = acf_current(tau)
												Next tau											

												For tau = 0 To 24
													acf_lib(tau) = acf_lib(50-tau)
												Next tau

												For lag = 0 To 18
													acf_teor(lag) = 0
													For tau = 0 To 50
														acf_teor(lag) += acf_lib(tau) * AmbigCoeff(tau, t, lag)
													Next tau
												Next lag

												d = 0
												If Config_sigma <> 0 Then
													For tau = 1 To 18
														If dat_all_str(h, t).var(tau) <> 0 Then
															d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2 / dat_all_str(h, t).var(tau)
														Else
															d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
														EndIf

													Next tau
												Else
													For tau = 1 To 18
														d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - acf_teor(tau)* (dat_all_str(h, t).acf(0)/acf_teor(0)) )^2
													Next tau
												EndIf

												Dim As Double ratio = 0

												If z < 2 Then
													dat_all_str(h, t).ratio = 0
												Else
													Dim As Double chi2constraint = (te - 2*dat_all_str(h-Hstep, t).te_c + dat_all_str(h-2*Hstep, t).te_c)^2 + _
													(ti - 2*dat_all_str(h-Hstep, t).ti_c + dat_all_str(h-2*Hstep, t).ti_c)^2
													If chi2constraint <> 0 Then
														ratio = chi2constraint/d
														d += Config_kappa*ratio
													Else
														ratio = 0
													EndIf
												EndIf

												If d < d_c(t) Then
													d_c(t) = d
													ti_c(t) = ti
													te_c(t) = te
													hyd_c(t) = hyd
													ratio_c(t) = ratio
												EndIf

											EndIf

										EndIf

									EndIf

								Next ti

							EndIf

						Next te

					EndIf

				EndIf

			Next t

		Next hyd

	EndIf
	
	Dim As Integer file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; hyd_c(t)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; ti_c(t)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; te_c(t)
	Next t
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; d_c(t)
	Next t
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Ratio."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; ratio_c(t)
	Next t
	Close #file
	
	
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
		
		For t = 0 To seans_num_out-1 ' �� �������
			
			If ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
				
				If (hyd >= dat_all_str(h, t).hyd_start) And (hyd <= dat_all_str(h, t).hyd_end) Then
					
					For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te
						
						If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) Then
							
							For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti
								
								If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) Then
									
									If (te/ti <= 3.5) And (te/ti >= 0.7) Then
										
										If acf_library_light_short_conv( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), 19) <> 0 Then
											
											d = 0
											For tau = 1 To 18
												d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
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
			
		Next t
		
	Next hyd
	
End Sub


''' ================================================================

Sub inverse_problem_v2(ByVal h As Integer, ByVal z As Integer, ByVal step_hyd As Integer, ByVal step_te As Integer, ByVal step_ti As Integer)
	
	Dim As Integer hyd
	Dim As Double  ti, te
	Dim As Integer tau
	Dim As Integer t
	Dim As Integer lag
	Dim As Double  d
	
	Dim As Double acf_lib(0 To 255)
	Dim As Double acf_teor(0 To 255)
	
	
	For hyd = 0 To libraries_num-1 Step step_hyd
		
		For t = 0 To seans_num_out-1 ' �� �������
			
			If heCurrent <= heRange(t) Then
				
				If ( hyd >= RegRange(4, t) ) And ( hyd <= RegRange(5, t) ) Then
					
					If (hyd >= dat_all_str(h, t).hyd_start) And (hyd <= dat_all_str(h, t).hyd_end) Then
						
						For te = dat_all_str(h, t).te_start To dat_all_str(h, t).te_end Step step_te
							
							If ( te >= RegRange(0, t) ) And ( te <= RegRange(1, t) ) Then
								
								For ti = dat_all_str(h, t).ti_start To dat_all_str(h, t).ti_end Step step_ti
									
									If ( ti >= RegRange(2, t) ) And ( ti <= RegRange(3, t) ) Then
										
										If (te/ti <= 3.5) And (te/ti >= 0.7) Then
											
											If acf_library_light_short( libraries_file(hyd), @temperatures(0), temperatures_len, ti, te, @acf_teor(0), num_point_acf) <> 0 Then
												
												If Config_triangle <> 0 Then
													For tau = 0 To num_point_acf-1
														acf_teor(tau) *= 1-tau*delta_tau/pulse_length
													Next tau
												EndIf
												
												func_conv_d(@acf_teor(0), @acf_filter(0), @acf_teor(0), num_point_acf)
												array_norm0_d(@acf_teor(0), @acf_teor(0), num_point_acf)
												
												d = 0
												For tau = 1 To 18
													d += Config_coeff(tau)*( dat_all_str(h, t).acf(tau) - dat_all_str(h, t).acf(0)*acf_teor(tau) )^2
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
				
			EndIf
			
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
	
	
	' �������� ��������� ������������� ��� Te
	For t = 0 To seans_num_out-1-RegWnd+1
		
		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).te_c
		Next i
		
		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(0, t+RegWnd/2) = RegMx - DeltaTeMinus - RegN*RegSigma
		RegRange(1, t+RegWnd/2) = RegMx + DeltaTePlus  + RegN*RegSigma
		
	Next t
	
	' ���������� ������ �������
	For t = 0 To RegWnd/2-1
		RegRange(0, t) = RegRange(0, RegWnd/2)
		RegRange(1, t) = RegRange(1, RegWnd/2)
	Next t
	
	' ���������� ����� �������
	For i = 0 To RegWnd/2-1
		RegRange(0, seans_num_out-1-i) = RegRange(0, seans_num_out-1-RegWnd/2)
		RegRange(1, seans_num_out-1-i) = RegRange(1, seans_num_out-1-RegWnd/2)
	Next i
	
	
	
	
	
	' �������� ��������� ������������� ��� Ti
	For t = 0 To seans_num_out-1-RegWnd+1
		
		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).ti_c
		Next i
		
		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(2, t+RegWnd/2) = RegMx - DeltaTiMinus - RegN*RegSigma
		RegRange(3, t+RegWnd/2) = RegMx + DeltaTiPlus  + RegN*RegSigma
	Next t
	
	' ���������� ������ �������
	For t = 0 To RegWnd/2-1
		RegRange(2, t) = RegRange(2, RegWnd/2)
		RegRange(3, t) = RegRange(3, RegWnd/2)
	Next t
	
	' ���������� ����� �������
	For i = 0 To RegWnd/2-1
		RegRange(2, seans_num_out-1-i) = RegRange(2, seans_num_out-1-RegWnd/2)
		RegRange(3, seans_num_out-1-i) = RegRange(3, seans_num_out-1-RegWnd/2)
	Next i
	
	
	
	
	' �������� ��������� ������������� ��� Hyd
	For t = 0 To seans_num_out-1-RegWnd+1
		
		For i = 0 To RegWnd-1
			RegArray(i) = dat_all_str(h, t+i).hyd_c
		Next i
		
		RegMx = stat_mean_d(@RegArray(0), RegWnd)
		RegSigma = stat_deviation_d(@RegArray(0), RegWnd)
		RegRange(4, t+RegWnd/2) = RegMx - DeltaHydMinus - RegN*RegSigma
		RegRange(5, t+RegWnd/2) = RegMx + DeltaHydPlus  + RegN*RegSigma
	Next t
	
	' ���������� ������ �������
	For t = 0 To RegWnd/2-1
		RegRange(4, t) = RegRange(4, RegWnd/2)
		RegRange(5, t) = RegRange(5, RegWnd/2)
	Next t
	
	' ���������� ����� �������
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
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ratio."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).ratio
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
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ratio."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; dat_all_str(h, t).ratio
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
	Print "���� ����������..."
	
	Do
		
		Print
		Print h, Hkm(h)
		Print "������� ����� ������� ��������� (�� 0 �� "; seans_num_out-1; " ): ";
		Input "", i_left
		
		Print "������� ������ ������� ��������� (�� "; i_left;" �� "; seans_num_out-1; " ): ";
		Input "", i_right
		
		Print "������� �������� He+ (� %): ";
		Input "", He
		
		If (i_right >= i_left) And (i_left >= 0) And (i_right <= (seans_num_out-1) ) Then
			
			If (He >= 0) And (He <= 100) Then
				
				' ��������� He
				For t = i_left To i_right
					dat_all_str(h, t).he_c = He
				Next t
				
				' ������� Te
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				
				For t = 0 To i_left-1
					Input #file, tmp_d
				Next
				
				For t = i_left To i_right
					Input #file, dat_all_str(h, t).te_c
				Next t
				
				Close #file
				
				' ������� Ti
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				
				For t = 0 To i_left-1
					Input #file, tmp_d
				Next
				
				For t = i_left To i_right
					Input #file, dat_all_str(h, t).ti_c
				Next t
				
				Close #file
				
				' ������� Hyd
				file = FreeFile()
				Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
				
				For t = 0 To i_left-1
					Input #file, tmp_d
				Next
				
				For t = i_left To i_right
					Input #file, dat_all_str(h, t).hyd_c
				Next t
				
				Close #file
				
				' ������� d_c
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
				Print "�������� He+ ������ ���� � �������� �� 0 �� 100%!"
				Color 15
				
			EndIf
			
		Else
			
			Color 12
			Print "������� ������� ������� ���������!"
			Color 15
			
		EndIf
		
		Print "���������� ���� ����������? (y/n) ";
		
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
	
	
	' �������� ���������� ���� Ti
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� Te
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� H+
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� ������������
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
	
	
	' �������� ���������� ���� Ti
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� Te
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� H+
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� ������������
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


Sub save_small_steps_results(ByVal h As Integer)
	
	Dim As Integer t, offset
	ReDim As Integer He_num(0 To He_max)
	Dim As Integer he, he_c
	Dim As Integer file
	Dim As Double d_c
	
	ReDim As Double d_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Ti_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Te_loaded(0 To He_max, 0 To seans_num_out-1)
	ReDim As Double Hyd_loaded(0 To He_max, 0 To seans_num_out-1)
	
	ReDim As Double Ti_c(0 To seans_num_out-1)
	ReDim As Double Te_c(0 To seans_num_out-1)
	ReDim As Double Hyd_c(0 To seans_num_out-1)
	ReDim As Double He_cs(0 To seans_num_out-1)
	ReDim As Double d_cs(0 To seans_num_out-1)
	
	
	' �������� ���������� ���� Ti
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Ti."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� Te
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Te."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� H+
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Hyd."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_loaded(he, t)
		Next t
		Close #file
	Next he
	
	' �������� ���������� ���� ������������
	For he = 0 To He_max Step he_step
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
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
		
		he_cs(t)  = he_c
		hyd_c(t) = Hyd_loaded(he_c, t)
		ti_c(t)  = Ti_loaded(he_c, t)
		te_c(t)  = Te_loaded(he_c, t)
		d_cs(t)   =  d_loaded(he_c, t)
		
	Next t
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; hyd_c(t)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; ti_c(t)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; te_c(t)
	Next t
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.D."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "##.#####^^^^^ "; d_cs(t)
	Next t
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "S.He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file, Using "###.# "; he_cs(t)
	Next t
	Close #file
	
	
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
	
	
	' �������� �������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file
	
	
	' �������� ���������� ���� ������������
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
		
		' ����� ������������
		
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_he(he, t)-d_loaded(t)
			Next t
		Next he
		
		' ����� ����������� ��������
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
		
		' ����� ����������� ���������
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
		
		
		
		' ����� Te
		
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
		
		
		' ����� H+
		
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
		
		
		' ����� He+
		
		
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
		
		
		' ����� Ti
		
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
		
		Draw String (300, 5), "h="+Left(Str( CInt(Hkm(h)) ), 4)+" ��", 12
		
		If isSaved = 1 Then
			Draw String (500, 5), "*", 13
		EndIf
		
		Draw String (900, 5), "F1 - ������", 4
		
		
		ScreenUnLock()
		
		key = GetKey()
		
		Select Case key
				
			Case KEY_CTRL_Q
				Cls
				Color 12
				Print "����� �� ���������? (y/n) "
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
					
					Input "������� ����� ��������� (0 - ������, 1 - Te, 2 - H+, 3 - He+, 4 - Ti, 5 - ��): ", wnd
					
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
				
				
				Input "������� ����� ��������� (0 - ������, 1 - Te, 2 - H+, 3 - He+, 4 - Ti, 5 - Te � Ti, 6 - H+ � Te � Ti): ", wnd
				
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
					
					Input "������� ����� ��������� (0 - ������, 3 - He+): ", wnd
					
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
				Input "������� ������ ���� ��� ���������� �� �������: ", wnd_trand_width
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
					
					Input "������� ����� ��������� (0 - ������, 1 - Te, 2 - H+, 4 - Ti): ", wnd
					
					If wnd = 0 Then
						Exit Select
					EndIf
					
					If wnd = 4 Then
						Input "������� ����� ������ (0 - ������, 1 - ������ ��������, 2 - ����������, 3 - �������� ������������, 4 - ������������� ���������,  5 - ������������� ��������� �� ���������� �����, 6 - ��������, 7 - Ti=Te (���� Ti>Te)): ", mode
					Else
						Input "������� ����� ������ (0 - ������, 1 - ������ ��������, 2 - ����������, 3 - �������� ������������, 4 - ������������� ���������,  5 - ������������� ��������� �� ���������� �����, 6 - ��������): ", mode
					EndIf
					
					If mode = 0 Then
						Exit Select
					EndIf
					
					Select Case wnd
							
						Case 1
							Select Case mode
									
								Case 1
									Dim As Integer level
									Input "������� �������� ���������: ", level
									level_te(h, z, point_start, point_end, level)
									
								Case 2
									Dim As Integer wnd_width
									Input "������� ������ ����: ", wnd_width
									trand_te(h, z, point_start, point_end, wnd_width)
									
								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_TE)
									
								Case 4
									Dim As Integer degree
									Input "������� ������� ��������: ", degree
									poly_te(h, z, point_start, point_end, degree)
									
								Case 5
									Dim As Integer wnd_width, degree
									Input "������� ������ ����: ", wnd_width
									Input "������� ������� ��������: ", degree
									spline_wnd_te(h, z, point_start, point_end, wnd_width, degree)
									
								Case 6
									gradient_te(h, z, point_start, point_end)
									
							End Select
							
						Case 2
							Select Case mode
								Case 1
									Dim As Integer level
									Input "������� �������� ��������� (%): ", level
									level_hyd(h, z, point_start, point_end, level)
									
								Case 2
									Dim As Integer wnd_width
									Input "������� ������ ����: ", wnd_width
									trand_hyd(h, z, point_start, point_end, wnd_width)
									
								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_H)
									
								Case 4
									Dim As Integer degree
									Input "������� ������� ��������: ", degree
									poly_hyd(h, z, point_start, point_end, degree)
									
							End Select
							
						Case 4
							Select Case mode
									
								Case 1
									Dim As Integer level
									Input "������� �������� ���������: ", level
									level_ti(h, z, point_start, point_end, level)
									
								Case 2
									Dim As Integer wnd_width
									Input "������� ������ ����: ", wnd_width
									trand_ti(h, z, point_start, point_end, wnd_width)
									
								Case 3
									interpolate_param(h, z, point_start, point_end, PARAM_TI)
									
								Case 4
									Dim As Integer degree
									Input "������� ������� ��������: ", degree
									poly_ti(h, z, point_start, point_end, degree)
									
								Case 5
									Dim As Integer wnd_width, degree
									Input "������� ������ ����: ", wnd_width
									Input "������� ������� ��������: ", degree
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
	Input "������� ������ ����: ", wnd_trand_width
	
	
	' �������� �������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file
	
	
	
	' �������� ���������� ���� He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he_loaded(t)
	Next t
	Close #file
	
	
	
	' �������� ���������� ���� ������������
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
		
		' ����� ������������
		
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_loaded(he, t)-d_loaded(He_max+1, t)
			Next t
		Next he
		
		' ����� ����������� ��������
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
		
		' ����� ����������� ���������
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
		Draw String (300, 5), "|�| = " + Left( Str( Abs(he_loaded(CUR)-CUR_HE) ), 3)+"%", 11
		
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
	
	Dim As Integer CUR_HE
	
	Cls
	Input "������� ������ ����: ", wnd_trand_width
	
	
	' �������� �������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "T.txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, t_loaded(t)
	Next t
	Close #file
	
	
	
	' �������� ���������� ���� He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he_loaded(t)
	Next t
	Close #file
	
	
	
	' �������� ���������� ���� ������������
	For he = 0 To He_max Step he_step
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(He)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, tmp_d(t)
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
		
		' ����� ������������
		
		For he = 0 To He_max Step he_step
			For t = 0 To seans_num_out-1
				tmp_d_d(he, t) = d_loaded(he, t)-d_loaded(He_max+1, t)
			Next t
		Next he
		
		' ����� ����������� ��������
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
		
		' ����� ����������� ���������
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
		Draw String (300, 5), "|�| = " + Left( Str( Abs(he_loaded(CUR)-CUR_HE) ), 3)+"%", 11
		
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
	Print "���������..."
	Color 15
	Sleep 400
	
End Sub


''' ================================================================

Function analysis_param(ByVal h As Integer, ByVal z As Integer, ByVal param As Integer) As Integer
	
	ReDim As Double param_loaded(0 To seans_num_out-1)
	ReDim As Double an(0 To seans_num_out-1)
	ReDim As Double bn(0 To seans_num_out-1)
	
	Dim As Integer t, f
	Dim As Integer file
	
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ param_str +Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	For t = 0 To seans_num_out-1
		an(0) += param_loaded(t)
	Next t
	an(0) /= seans_num_out
	an(0) /= 2
	
	For f = 1 To seans_num_out\2
		For t = 0 To seans_num_out-1
			an(f) += param_loaded(t)*Cos(f*t*2*(4*Atn(1))/seans_num_out)
			bn(f) += param_loaded(t)*Sin(f*t*2*(4*Atn(1))/seans_num_out)
		Next t
		an(f) /= seans_num_out
		bn(f) /= seans_num_out
		
	Next f
	
	
	' ������ ���������� ����
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/W."+ param_str+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	
	For f = 0 To seans_num_out\2
		Print #file, Using "##.####^^^^ "; Sqr(an(f)^2+bn(f)^2)
	Next f
	Close #file
	
	Dim As Double abMax = -1e200
	Dim As Integer abMaxIndex = 0
	For f = 0 To seans_num_out\2
		If Sqr(an(f)^2+bn(f)^2) > abMax Then
			abMax = Sqr(an(f)^2+bn(f)^2)
			abMaxIndex = f
		EndIf
	Next f
	
	For f = 0 To seans_num_out\2
		If Sqr(an(f)^2+bn(f)^2)/Sqr(an(abMaxIndex)^2+bn(abMaxIndex)^2) < 0.05 Then
			
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/F."+ param_str+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
			Print #file, f
			Close #file
			
			file = FreeFile()
			Open SEANS_DIR_OUT + DirectoryOutput+"/step3/N."+ param_str+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
			Print #file, seans_num_out\f
			Close #file
			
			Return seans_num_out\f
			
		EndIf
	Next f
	
	
End Function

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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ param_str +Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	d_param = ( param_loaded(t_end)-param_loaded(t_start) ) / (t_end-t_start)
	
	
	If (param = PARAM_TI) Or (param = PARAM_TE) Then
		For t = t_start To t_end ' �� �������
			param_loaded(t) =  ( ( CInt( param_loaded(t_start) + d_param*(t-t_start) ) ) \ 20 ) * 20
		Next t
	Else
		For t = t_start To t_end ' �� �������
			param_loaded(t) =  CInt( param_loaded(t_start) + d_param*(t-t_start) )
		Next t
	EndIf
	
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) =   ( CInt( trand(t) ) \ 20 ) * 20
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) =   ( CInt( trand(t) ) \ 20 ) * 20
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	array_trand_d(@param_loaded(0), @trand(0), wnd, seans_num_out)
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) =   CInt( trand(t) )
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) =  ( CInt( level ) \ 20 ) * 20
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) =  ( CInt( level ) \ 20 ) * 20
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	For t = t_start To t_end ' �� �������
		param_loaded(t) = CInt( level*2 )
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
		' �������� ���������� ���� ���������� ���������
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te2."+ Str(-1)+"."+Str(CInt(Hkm(h-Hstep*(zz))))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, param_loaded(z-zz, t)
		Next t
		Close #file
	Next zz
	
	For zz As Integer = 0 To z-2
		gradients(zz) = 0
		For t = t_start To t_end ' �� �������
			gradients(zz) += param_loaded(zz+1, t)-param_loaded(zz, t)
		Next t
		gradients(zz) /= t_end-t_start
	Next zz
	
	Cls
	
	For zz As Integer = 0 To z-2
		Print Using "#### "; gradients(zz);
	Next zz
	Print
	
	Input "������� �������� ���������: ", gradient
	
	For t = t_start To t_end ' �� �������
		param_loaded(z, t) =  ( CInt( param_loaded(z-1, t) + gradient ) \ 20 ) * 20
	Next t
	
	' ������ ���������� ���� ���������� ���������
	
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	Dim As Integer y = 0
	For t = t_start To t_end ' �� �������
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t
	
	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)
	
	y = 0
	For t = t_start To t_end ' �� �������
		
		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k
		
		param_loaded(t) = ( CInt( param_loaded(t) ) \ 20 ) * 20
		
		y += 1
		
	Next t
	
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	Dim As Integer y = 0
	For t = t_start To t_end ' �� �������
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t
	
	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)
	
	y = 0
	For t = t_start To t_end ' �� �������
		
		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k
		
		param_loaded(t) = ( CInt( param_loaded(t) ) \ 20 ) * 20
		
		y += 1
		
	Next t
	
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Hyd."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	Dim As Integer y = 0
	For t = t_start To t_end ' �� �������
		in_x(t) = y
		in_y(t) = param_loaded(t)
		y += 1
	Next t
	
	approx_poly_coeff_d(@in_x(0), @in_y(0), y, @coeff(0), degree)
	
	y = 0
	For t = t_start To t_end ' �� �������
		
		param_loaded(t) = 0
		For k As Integer = 0 To degree
			param_loaded(t) += coeff(k) * y^k
		Next k
		
		param_loaded(t) = CInt( param_loaded(t) )
		
		y += 1
		
	Next t
	
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	
	Dim As Integer y = 0
	For t = t_start To t_end ' �� �������
		
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
	For t = t_start+1 To t_end-1 ' �� �������
		param_loaded(t) =   ( CInt( out_y(y) ) \ 20 ) * 20
		y += 1
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	
	' �������� ���������� ���� ���������� ���������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, param_loaded(t)
	Next t
	Close #file
	
	
	
	Dim As Integer y = 0
	For t = t_start To t_end ' �� �������
		
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
	For t = t_start+1 To t_end-1 ' �� �������
		param_loaded(t) =   ( CInt( out_y(y) ) \ 20 ) * 20
		y += 1
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
		
		' �������� Ti
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, ti_all(he, t)
		Next t
		Close #file
		
		' �������� Te
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, te_all(he, t)
		Next t
		Close #file
		
		' �������� H+
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, hyd_all(he, t)
		Next t
		Close #file
		
		' �������� ������������
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
		For t = 0 To seans_num_out-1
			Input #file, d_all(he, t)
		Next t
		Close #file
		
	Next he
	
	
	param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
	
	d_he = ( he_loaded(t_end)-he_loaded(t_start) ) / (t_end-t_start)
	
	
	For t = t_start To t_end ' �� �������
		he_loaded(t) = CInt( he_loaded(t_start) + d_he*(t-t_start) )
	Next t
	
	
	For t = t_start+1 To t_end-1 ' �� �������
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
	
	' �������� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file
	
	For he = 0 To he_max Step he_step
		
		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,
		
		' ��������� ���������� ���
		
		Print "�������� ��������� ���... ";
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		libraries_list_load(He)
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
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
			
			dat_all_str(h, t).ti_start = ti_loaded(t)
			dat_all_str(h, t).ti_end   = ti_loaded(t)
			
			dat_all_str(h, t).te_start = 500
			dat_all_str(h, t).te_end   = 4000
			
			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1
			
			dat_all_str(h, t).d_c = 1e200 ' ������� ������������
			
		Next t
		
		
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			Else
				inverse_problem_v2(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			EndIf
		EndIf
		
		
		' 3 ���
		ranges_set(h, Config_range_h_3, Config_range_te_3, 0)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		
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
	
	' �������� Te
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file
	
	For he = 0 To he_max Step he_step
		
		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,
		
		' ��������� ���������� ���
		Print "�������� ��������� ���... ";
		
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		libraries_list_load(He)
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
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
			
			dat_all_str(h, t).te_start = te_loaded(t)
			dat_all_str(h, t).te_end   = te_loaded(t)
			
			dat_all_str(h, t).ti_start = 500
			dat_all_str(h, t).ti_end   = 4000
			
			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1
			
			dat_all_str(h, t).d_c = 1e200 ' ������� ������������
			
		Next t
		
		
		
		
		
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			Else
				inverse_problem_v2(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			EndIf
		EndIf
		
		' 3 ���
		ranges_set(h, Config_range_h_3, 0, Config_range_ti_3)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		
		
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
	
	' �������� Hyd
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd_loaded(t)
	Next t
	Close #file
	
	
	For he = 0 To he_max Step he_step
		
		Print Using "n = ###   h = ####   He+ = ##"; h; Hkm(h); he;
		Print ,
		' ��������� ���������� ���
		
		Print "�������� ��������� ���... ";
		
		
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		libraries_list_load(He)
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
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
			
			dat_all_str(h, t).d_c = 1e200 ' ������� ������������
			
		Next t
		
		
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, 1, Config_step_te_2, Config_step_ti_2)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, 1, Config_step_te_2, Config_step_ti_2)
			Else
				inverse_problem_v2(h, z, 1, Config_step_te_2, Config_step_ti_2)
			EndIf
		EndIf
		
		
		' 3 ���
		ranges_set(h, 0, Config_range_te_3, Config_range_ti_3)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, 1, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, 1, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, 1, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		
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
		
		Print "�������� ��������� ���... ";
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		
		libraries_list_load(He)
		
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
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
			
			dat_all_str(h, t).ti_start = ti_loaded(t)
			dat_all_str(h, t).ti_end   = ti_loaded(t)
			
			dat_all_str(h, t).te_start = te_loaded(t)
			dat_all_str(h, t).te_end   = te_loaded(t)
			
			dat_all_str(h, t).hyd_start = hyd_loaded(t)
			dat_all_str(h, t).hyd_end   = hyd_loaded(t)
			
			dat_all_str(h, t).d_c = 1e200 ' ������� ������������
			
		Next t
		
		
		' 3 ���
		'ranges_set(h, 0, 0, 0)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		
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
		
		' ��������� ���������� ���
		Print "�������� ��������� ���... ";
		
		If libraries_num > 0 Then
			For hyd = 0 To libraries_num-1
				fclose ( libraries_file(hyd) )
			Next hyd
		EndIf
		
		libraries_list_load(He)
		
		If Config_oxygen <> 0 Then
			libraries_num = 1
		EndIf
		
		For hyd = 0 To libraries_num-1
			Print_process_percent((hyd*100)/libraries_num)
			library_light_list_get_filename(@zfilename, @libraries_filelist, hyd)
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
			
			dat_all_str(h, t).ti_start = ti_loaded(t)
			dat_all_str(h, t).ti_end   = ti_loaded(t)
			
			dat_all_str(h, t).te_start = te_loaded(t)
			dat_all_str(h, t).te_end   = te_loaded(t)
			
			dat_all_str(h, t).hyd_start = 0
			dat_all_str(h, t).hyd_end   = libraries_num-1
			
			dat_all_str(h, t).d_c = 1e200 ' ������� ������������
			
		Next t
		
		
		' 2 ���
		'ranges_set(h, Config_step_h_2, 0, 0)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			Else
				inverse_problem_v2(h, z, Config_step_h_2, Config_step_te_2, Config_step_ti_2)
			EndIf
		EndIf
		
		' 3 ���
		ranges_set(h, Config_range_h_3, 0, 0)
		If isConv <> 0 Then
			inverse_problem_v2_conv(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
		Else
			If ConfigAmbig <> 0 Then
				inverse_problem_v2_ambig(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			Else
				inverse_problem_v2(h, z, Config_step_h_3, Config_step_te_3, Config_step_ti_3)
			EndIf
		EndIf
		
		results_write2(h, he)
		
	Next he
	
	intervals_input_auto2(h)
	
End Sub


''' ================================================================

Sub param_load(ByVal h As Integer, ByVal ti As Double Ptr, ByVal te As Double Ptr, ByVal hyd As Double Ptr, ByVal he As Double Ptr, ByVal d As Double Ptr)
	
	Dim As Integer t, file
	
	' �������� ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti[t]
	Next t
	Close #file
	
	' �������� ���������� ���� Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� D
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
	
	' �������� ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti[t]
	Next t
	Close #file
	
	' �������� ���������� ���� Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He2."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, he[t]
	Next t
	Close #file
	
	
	' �������� ���������� ���� D
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
	
	' ������ ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "####.# ";ti[t]
	Next t
	Close #file
	
	' ������ ���������� ���� Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "####.# ";te[t]
	Next t
	Close #file
	
	
	' ������ ���������� ���� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "###.# "; hyd[t]
	Next t
	Close #file
	
	
	' ������ ���������� ���� He+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "He."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "###.# "; he[t]
	Next t
	Close #file
	
	
	' ������ ���������� ���� D
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
	
	' �������� ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_loaded(t)
	Next t
	Close #file
	
	' �������� ���������� ���� Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Te."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_loaded(t)
	Next t
	Close #file
	
	
	For t = t_start To t_end ' �� �������
		If ti_loaded(t) > te_loaded(t) Then
			ti_loaded(t) = te_loaded(t)
		EndIf
	Next t
	
	' ������ ���������� ���� ���������� ���������
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
	Print "UPRISE version " + UPRISE_VERSION
	Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
	Print
	Color 7
	Print "Estimate - ��������� ������ ���������� ��������� (������� �������� ������ ���������)"
	Print "(c) ������� �.�., ����� �.�. (�������� ���������)"
	Color 8
	Print
	Print "================================"
	Print "��������� ������� " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
	Print "================================"
	Color 15
	Print
	Print "����������:"
	Print " Alt + Enter      ������������� �� �������� ������ � ������������� � ��������"
	Print "       Left/Right ����������� �� �������"
	Print "       Home       ����������� ������� � ������ ������"
	Print "       End        ����������� ������� � ����� ������"
	Print "       Tab        ����������� ������� �� 10 ����� ������"
	Print "Ctrl + Left/Right ����� �������� �� ��� �������"
	Print "       ������     ������/�������� ����� ������ ��� ������������, ���������, ������ ���������"
	Print "       F1         ����� ���� ������"
	Print "Ctrl + N          ������� �� ��������� ������"
	Print "       I          ������������ ������ ��� ���� ����������"
	Print "       R          �������� ���������� �� ������ ������������������"
	Print "       P          ���������� ��������� ��������� ��������� (������ ���������)"
	Print "       U          ������ ����������� �������� (������������, ���������, ������ ���������)"
	Print "       T          �������� ������ ���� ��� ������� ������ ���������� �� ���������� ������"
	Print
	Color 12
	Print "������� Enter"
	
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
	
	Input "������� �������� He+: "; he
	
	' �������� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti_he(t)
	Next t
	Close #file
	
	' �������� Te
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Te."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, te_he(t)
	Next t
	Close #file
	
	' �������� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd_he(t)
	Next t
	Close #file
	
	' �������� ������������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "D."+Str(he)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, d_he(t)
	Next t
	Close #file
	
	
	param_load(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
	
	
	For t = t_start+1 To t_end-1 ' �� �������
		he_loaded(t) = he
	Next t
	
	For t = t_start+1 To t_end-1 ' �� �������
		d_loaded(t)   = d_he(t)
		ti_loaded(t)  = ti_he(t)
		te_loaded(t)  = te_he(t)
		hyd_loaded(t) = hyd_he(t)
	Next t
	
	param_save(h, @ti_loaded(0), @te_loaded(0), @hyd_loaded(0), @he_loaded(0), @d_loaded(0) )
	
End Sub

''' ================================================================

Function getHydMax(h As Integer) As Integer
	
	Dim As Integer t
	Dim As Integer file
	ReDim As Double hyd(0 To seans_num_out-1)
	Dim As Double hydMax
	
	' �������� ���������� ���� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	
	For t = 0 To seans_num_out-1
		Input #file, hyd(t)
	Next t
	Close #file
	
	hydMax = hyd(0)
	For t = 1 To seans_num_out-1
		If hyd(t) > hydMax Then
			hydMax = hyd(t)
		EndIf
	Next t
	
	Return hydMax
	
End Function

''' ================================================================

Function getHeMax(h As Integer) As Integer
	
	Dim As Integer t
	Dim As Integer file
	ReDim As Double he(0 To seans_num_out-1)
	Dim As Double heMax
	
	' �������� ���������� ���� He+
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


Sub libraries_list_load(He As Integer)
	
	
	If pulse_length = 663 Then
		
		If isConv = 1 Then
			libraries_num = library_light_list_get("Short_663_P+C_", He, @libraries_filelist) ' �������� ������ ��������� ��� He=0 � ����� ���� ���������
		Else
			libraries_num = library_light_list_get("For_Ambig+q_profile_", He, @libraries_filelist) ' �������� ������ ��������� ��� He=0 � ����� ���� ���������
		EndIf
		
	EndIf
	
	If pulse_length = 795 Then
		
		If isConv = 1 Then
			libraries_num = library_light_list_get("Short_795_P+C_", He, @libraries_filelist) ' �������� ������ ��������� ��� He=0 � ����� ���� ���������
		Else
			libraries_num = library_light_list_get("For_Ambig+q_profile_", He, @libraries_filelist) ' �������� ������ ��������� ��� He=0 � ����� ���� ���������
		EndIf
		
	EndIf
	
End Sub


''' ================================================================


Sub save_and_exit()
	
	
	Dim As Integer nH, nT
	Dim As Integer t, h
	Dim As Integer file
	Dim As Double temp
	
	
	' �������� ���������� �������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/T.txt" For Input As #file
	
	nT = 0
	Do While Not Eof(file)
		Input #file, temp
		nT += 1
	Loop
	
	Close #file
	
	
	
	' �������� ���������� �����
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/H.txt" For Input As #file
	
	nH = 0
	Do While Not Eof(file)
		Input #file, temp
		nH += 1
	Loop
	
	Close #file
	
	
	' �������� ������
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)
	ReDim As Double  q_array  (0 To nT-1, 0 To nH-1)
	ReDim As Double  ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd_array(0 To nT-1, 0 To nH-1)
	ReDim As Double  m_array  (0 To nT-1, 0 To nH-1)
	
	
	' ��������� ����� �������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/T.txt" For Input As #file
	
	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t
	
	Close #file
	
	
	' ��������� �������� �����
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/H.txt" For Input As #file
	
	
	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h
	
	Close #file
	
	
	For i As Integer = 0 To nH-1
		For j As Integer = 0 To nH-1-1
			If h_array(j) > h_array(j+1) Then
				Dim As Integer tmp = h_array(j)
				h_array(j) = h_array(j+1)
				h_array(j+1) = tmp
			EndIf
		Next j
	Next i
	
	
	
	ReDim As Double  _ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _hyd_array(0 To nT-1, 0 To nH-1)
	
	ReDim As Double  ti2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd2_array(0 To nT-1, 0 To nH-1)
	
	ReDim As Double  ti_s_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te_s_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he_s_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd_s_array(0 To nT-1, 0 To nH-1)
	
	' ������ ������ �� �����
	For h = 0 To nH-1
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Q."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, q_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, hyd_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/M.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, m_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/He.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, he_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _ti_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _te_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _hyd_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_He.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _he_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti2_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te2_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Hyd2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, hyd2_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/He2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, he2_array(t, h)
		Next t
		Close #file
		
		
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti_s_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te_s_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, hyd_s_array(t, h)
		Next t
		Close #file
		
		file = FreeFile()
		Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.He.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, he_s_array(t, h)
		Next t
		Close #file
		
	Next h
	
	
	
	' ������ Ti
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Ti.txt" For Output As #file
	
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
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Ti.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; ti_s_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Ti.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; _ti_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
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
	
	
	' ������ Te
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Te.txt" For Output As #file
	
	
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
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Te.txt" For Output As #file
	
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; te_s_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Te.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###### "; _te_array(t, h);
		Next h
	Next t
	
	Close #file
	
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
	
	' ������ He
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/He.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; he_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.He.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; he_s_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_He.txt" For Output As #file
	
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; _he_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/He2.txt" For Output As #file
	
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; he2_array(t, h);
		Next h
	Next t
	
	Close #file
	
	' ������ Hyd
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Hyd.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; hyd_array(t, h)/2;
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/S.Hyd.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; hyd_s_array(t, h);
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/_Hyd.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; _hyd_array(t, h)/2;
		Next h
	Next t
	
	Close #file
	
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Hyd2.txt" For Output As #file
	
	Print #file, "      0 ";
	
	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; hyd2_array(t, h)/2;
		Next h
	Next t
	
	Close #file
	
	
	
	' ������ Q
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Q.txt" For Output As #file
	
	Print #file, "      0 ";
	
	h = Config_h_min_q
	Do
		Print #file, Using " ###### "; Hkm(h);
		h += 1
	Loop Until Hkm(h) > h_array(nH-1)
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		h = Config_h_min_q
		Do
			Print #file, Using "###.### "; dat_all_str(h, t).q;
			h += 1
		Loop Until Hkm(h) > h_array(nH-1)
	Next t
	
	Close #file
	
	
	
	
	' ������ Qh2
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3"+"/Qh2.txt" For Output As #file
	
	
	Print #file, "      0 ";
	
	h = Config_h_min_q
	Do
		Print #file, Using "###### "; Hkm(h);
		h += 1
	Loop Until Hkm(h) > h_array(nH-1)
	
	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		h = Config_h_min_q
		Do
			Print #file, Using "##.###^^^^ "; dat_all_str(h, t).q*(Hkm(h)^2);
			h += 1
		Loop Until Hkm(h) > h_array(nH-1)
	Next t
	
	Close #file
	
	sqlite3_close(db)
	Kill(database_name)
	
End Sub


''' ================================================================


Sub interpolate_ti(ByVal h As Integer, ByVal z As Integer)
	
	ReDim As Integer label(0 To seans_num_out-1)
	
	ReDim As Double ti_trand(0 To seans_num_out-1)
	ReDim As Double hyd_trand(0 To seans_num_out-1)
	
	ReDim As Double ti(0 To seans_num_out-1)
	ReDim As Double hyd(0 To seans_num_out-1)
	
	ReDim As Double prod(0 To seans_num_out-1)
	
	Dim As Double mean, dev
	
	Dim As Integer file, t, i
	
	
	FileCopy (SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt", SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "+Ti."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt")
	
	' �������� ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti(t)
	Next t
	Close #file
	
	' �������� ���������� ���� H+
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Hyd."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, hyd(t)
	Next t
	Close #file
	
	array_trand_d(@ti(0), @ti_trand(0), Config_ti_interpolate_num, seans_num_out)
	array_trand_d(@hyd(0), @hyd_trand(0), Config_ti_interpolate_num, seans_num_out)
	
	
	For t = 0 To seans_num_out-1
		prod(t) = -hyd(t)*ti(t)
	Next t
	
	
	mean = stat_mean_d(@prod(0), seans_num_out)
	dev = stat_deviation_d(@prod(0), seans_num_out)
	
	
	For t = 0 To seans_num_out-1
		if Abs(prod(t)-mean) > Config_ti_interpolate_dev*dev Then
			label(t) = 1
		Else
			label(t) = 0
		EndIf
	Next t
	
	
	
	' �������� ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Input As #file
	For t = 0 To seans_num_out-1
		Input #file, ti(t)
	Next t
	Close #file
	
	For t = 0 To seans_num_out-1
		
		Dim As Integer c = 0
		Dim As Double d = 0
		
		
		i = 0
		Do While (c < Config_ti_interpolate_points\2)
			
			If t-i < 0 Then
				Exit Do
			EndIf
			
			If label(t-i) = 1 Then
				c += 1
				d += ti(t-i)
			EndIf
			
			i += 1
			
		Loop
		
		i = 0
		Do While (c < Config_ti_interpolate_points\2)
			
			If t+i > seans_num_out-1 Then
				Exit Do
			EndIf
			
			If label(t+i) = 1 Then
				c += 1
				d += ti(t+i)
			EndIf
			
			i += 1
			
		Loop
		
		If c <> 0 Then
			ti(t) = ( CInt(d/c) \ 20 ) * 20
		EndIf
		
	Next t
	
	
	' ������ ���������� ���� ������������ ��������
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Var."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "#######.# "; prod(t)
	Next t
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Mean+Dev."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	Print #file,  Using "#######.#  #######.# "; mean; dev
	Close #file
	
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/"+ "Label."+Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	For t = 0 To seans_num_out-1
		Print #file,  Using "# "; label(t)
	Next t
	Close #file
	
	' ������ ���������� ���� Ti
	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step3/Ti."+ Str(-1)+"."+Str(CInt(Hkm(h)))+".txt" For Output As #file
	
	For t = 0 To seans_num_out-1
		Print #file, Using "####.# "; ti(t)
	Next t
	Close #file
	
End Sub

''' ================================================================

