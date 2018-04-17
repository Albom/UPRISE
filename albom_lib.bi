
#Include Once "crt.bi"

''' ===================================================================================================

#Ifndef NULL
#Define NULL 0
#EndIf

#Ifndef M_PI
#Define M_PI 3.14159265358979323846
#EndIf

#Ifndef M_E
#Define 2.7182818284590452354
#EndIf

''' ===================================================================================================
' Подключаем albom.dll

#Inclib "./albom"

''' ===================================================================================================
' описание scan-кодов клавиш и комбинация клавиш

Enum AlbomKeys
	KEY_CTRL_A = 1
	KEY_BACKSPACE = 8
	KEY_TAB = 9
	KEY_ENTER = 13
	KEY_CTRL_N = 14
	KEY_CTRL_P = 16
	KEY_CTRL_Q = 17
	KEY_CTRL_R = 18
	KEY_CTRL_S = 19
	KEY_CTRL_U = 21
	KEY_ESC = 27
	KEY_SPACE = 32
	KEY_PLUS = 43
	KEY_MINUS = 45
	KEY_1 = 49
	KEY_2 = 50
	KEY_3 = 51
	KEY_4 = 52
	KEY_A_CAPITAL = 65
	KEY_P_CAPITAL = 80
	KEY_R_CAPITAL = 82
	KEY_W_CAPITAL = 87
	KEY_Y_CAPITAL = 89
	KEY_A = 97
	KEY_B = 98
	KEY_C = 99
	KEY_D = 100
	KEY_E = 101
	KEY_F = 102
	KEY_G = 103
	KEY_H = 104
	KEY_I = 105
	KEY_M = 109
	KEY_N = 110
	KEY_O = 111
	KEY_P = 112
	KEY_Q = 113
	KEY_R = 114
	KEY_S = 115
	KEY_T = 116
	KEY_U = 117
	KEY_V = 118
	KEY_W = 119
	KEY_Y = 121
	KEY_Z = 122
	KEY_F1 = 15359
	KEY_F11 = 34303
	KEY_F12 = 34559
	KEY_LEFT = 19455
	KEY_RIGHT = 19967
	KEY_UP = 18687
	KEY_DOWN = 20735
	KEY_DEL = 21503
	KEY_CTRL_DEL = 37887
	KEY_CTRL_LEFT = 29695
	KEY_CTRL_RIGHT = 29951
	KEY_CTRL_UP = 36351
	KEY_CTRL_DOWN = 37375
	KEY_PAGE_UP = 18943
	KEY_PAGE_DOWN = 20991
	KEY_HOME = 18431
	KEY_END = 20479
	KEY_CTRL_PAGE_UP = 34047
	KEY_CTRL_PAGE_DOWN = 30463
End Enum



''' ===================================================================================================
' Функция Print_process - Выводит вращающийся указатель


Function Print_process(ByVal num As Integer) As Integer
	Select Case num
		Case 0: Print "|"; : Locate ,Pos-1: Return num+1
		Case 1: Print "/"; : Locate ,Pos-1: Return num+1
		Case 2: Print "-"; : Locate ,Pos-1: Return num+1
		Case 3: Print "\"; : Locate ,Pos-1: Return 0
	End Select
End Function

''' ===================================================================================================
' Функция Print_process - Выводит количество процентов

Sub Print_process_percent(ByVal num As Integer)
	Dim As String s
	If num > 100 Then
		Print "    ";
		Locate ,Pos-4
	Else
		s = Str(num)
		If num < 10  Then s = "0"+s
		If num < 100 Then s = "0"+s

		Print s;"%";
		Locate ,Pos-4
	EndIf
End Sub


''' ===================================================================================================
' Процедура break - Ожидает нажатия клавиши и завершает работу программы

Sub break()
	Sleep
	End
End Sub



''''' ===================================================================================================
' Ф-ция возвращает 1, если нажата Y
' или 0, если нажата N

Function GetKey_YN() As Integer
	Dim key As Integer
	Do
		key = GetKey
	Loop Until key = KEY_Y Or key = KEY_N

	If key = KEY_Y Then Print "Y": Return 1 Else Print "N": Return 0
End Function



''' ===================================================================================================
' заголовок старых и новых S-файлов, а также V- и C- файлов

Type  seans_header Field = 1

	Dim Day1							As Integer
	Dim Month1						As Integer
	Dim Year1						As Integer
	Dim Hour1						As Integer
	Dim Minute1						As Integer
	Dim Second1						As Integer

	Dim NSeans						As Integer
	Dim Nr0							As Integer
	Dim Nr1							As Integer

	Dim U1							As Integer

End Type

''' ===================================================================================================
''' seans.dll

Type  seans1s_data Field = 1

	Dim Day1							As Integer
	Dim Month1						As Integer
	Dim Year1						As Integer
	Dim Hour1						As Integer
	Dim Minute1						As Integer
	Dim Second1						As Integer

	Dim NSeans						As Integer
	Dim Nr0							As Integer
	Dim Nr1							As Integer

	Dim U1							As Integer

	Dim Dat(0 To 169, 0 To 17) As Integer

	Dim Datm(0 To 679)			As Integer
	Dim Datp(0 To 679)			As Integer

	Dim m(0 To 679)				As Integer

End Type

Type  seans1c_data Field = 1

	Dim Day1							As Integer
	Dim Month1						As Integer
	Dim Year1						As Integer
	Dim Hour1						As Integer
	Dim Minute1						As Integer
	Dim Second1						As Integer

	Dim NSeans						As Integer
	Dim Nr0							As Integer
	Dim Nr1							As Integer

	Dim U1							As Integer

	Dim Dat1(0 To 359, 0 To 6)	As Integer
	Dim Dat2(0 To 359, 0 To 6)	As Integer
	Dim Dat0(0 To 359)			As Integer

	Dim m(0 To 359)				As Integer

End Type


Extern "c"
Declare Function  seans1s_test Alias "seans1s_test" ( ByVal filename As ZString Ptr) As Integer

Declare Function  seans1s_load_header Alias "seans1s_load_header" (_
ByVal filename As ZString Ptr, _
ByVal header As seans_header Ptr _
) As Integer


Declare Function  seans1s_load Alias "seans1s_load" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans1s_data Ptr _
) As Integer

Declare Function  seans1s_save Alias "seans1s_save" ( _
ByVal filename As ZString Ptr, _
ByVal seans As seans1s_data Ptr _
) As Integer

Declare Function  seans1s_saveM0 Alias "seans1s_saveM0" (_
ByVal filename As ZString Ptr _
) As Integer

Declare Function  seans1s_saveM3 Alias "seans1s_saveM3" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans1s_data Ptr _
) As Integer


Declare Function  seans1s_alt Alias "seans1s_alt" (ByVal H As Integer) As Double
Declare Function  seans1s_alt_front Alias "seans1s_alt_front" (ByVal H As Integer) As Double
Declare Function  seans1s_alt_795 Alias "seans1s_alt_795" (ByVal H As Integer) As Double


Declare Function  seans1s_noise Alias "seans1s_noise" (_
ByVal seans As seans1s_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer

Declare Function  seans1c_alt Alias "seans1c_alt" (ByVal H As Integer) As Double

Declare Function  seans1c_test Alias "seans1c_test" ( ByVal filename As ZString Ptr) As Integer

Declare Function  seans1cv_load Alias "seans1cv_load" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans1c_data Ptr _
) As Integer

Declare Function  seans1c_load Alias "seans1c_load" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans1c_data Ptr _
) As Integer

Declare Function  seans1c_noise1 Alias "seans1c_noise1" (_
ByVal seans As seans1c_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer

Declare Function  seans1c_noise2 Alias "seans1c_noise1" (_
ByVal seans As seans1c_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer

Declare Function  seans1c_saveM0 Alias "seans1c_saveM0" (_
ByVal filename As ZString Ptr _
) As Integer

Declare Function  seans1c_saveM3 Alias "seans1c_saveM3" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans1c_data Ptr _
) As Integer

End Extern



''' ===================================================================================================
''' seans2

Type  seans2_data Field = 1

	Dim Day1								As Integer
	Dim Month1							As Integer
	Dim Year1							As Integer
	Dim Hour1							As Integer
	Dim Minute1							As Integer
	Dim Second1							As Integer

	Dim NSeans							As Integer
	Dim Nr0								As Integer
	Dim Nr1								As Integer

	Dim U1								As Integer

	Dim Dat1(0 To 679, 0 To 18)	As Integer
	Dim Dat2(0 To 679, 0 To 18)	As Integer
	Dim Dat3(0 To 679, 0 To 18)	As Integer
	Dim Dat4(0 To 679, 0 To 18)	As Integer

	Dim Dat01(0 To 679)				As Integer
	Dim Dat02(0 To 679)				As Integer

	Dim Datps1(0 To 679)				As Integer
	Dim Datps2(0 To 679)				As Integer

	Dim Dat03(0 To 679)				As Integer
	Dim Dat04(0 To 679)				As Integer

	Dim M(0 To 679)					As Integer

End Type

Extern "c"
Declare Function  seans2_test Alias "seans2_test" ( _
ByVal filename As ZString Ptr _
) As Integer

Declare Function  seans2_load_header Alias "seans2_load_header" (_
ByVal filename As ZString Ptr, _
ByVal header As seans_header Ptr _
) As Integer

Declare Function  seans2_load Alias "seans2_load" ( _
ByVal filename As ZString Ptr, _
ByVal seans As seans2_data Ptr _
) As Integer

Declare Function  seans2_save Alias "seans2_save" ( _
ByVal filename As ZString Ptr, _
ByVal seans As seans2_data Ptr _
) As Integer

Declare Function  seans2_altL Alias "seans2_altL" ( _
ByVal H As Integer _
) As Double

Declare Function  seans2_altS Alias "seans2_altS" ( _
ByVal H As Integer _
) As Double

Declare Function  seans2_altL_front Alias "seans2_altL_front" ( _
ByVal H As Integer _
) As Double

Declare Function  seans2_saveM0 Alias "seans2_saveM0" (_
ByVal filename As ZString Ptr _
) As Integer


Declare Function  seans2_saveM3 Alias "seans2_saveM3" (_
ByVal filename As ZString Ptr, _
ByVal seans As seans2_data Ptr _
) As Integer

Declare Function  seans2_noise1 Alias "seans2_noise1" (ByVal seans As seans2_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer

Declare Function  seans2_noise3 Alias "seans2_noise3" (ByVal seans As seans2_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer


Declare Function  seans2_noise2 Alias "seans2_noise2" (ByVal seans As seans2_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer

Declare Function  seans2_noise4 Alias "seans2_noise4" (ByVal seans As seans2_data Ptr, _
ByVal acf_noise As Double Ptr, _
ByVal length As Integer, _
ByVal alt_start As Integer, _
ByVal alt_end As Integer _
) As Integer


End Extern


''' ===================================================================================================
''' seans3

Type  seans3_data Field = 1

	Dim Day1		As Integer
	Dim Month1	As Integer
	Dim Year1	As Integer
	Dim Hour1	As Integer
	Dim Minute1	As Integer
	Dim Second1	As Integer

	Dim NP		As Integer
	Dim NR		As Integer

	Dim Freq		As Double

	Dim Magic	As Integer

	Dim Dat1		As Integer Ptr
	Dim Dat2		As Integer Ptr
	Dim Dat3		As Integer Ptr
	Dim Dat4		As Integer Ptr

End Type

Extern "c"

Declare Function seans3_load Alias "seans3_load" ( ByVal filename As ZString Ptr, ByVal seans As seans3_data Ptr) As Integer
Declare Function seans3_close Alias "seans3_close" (ByVal seans As seans3_data Ptr) As Integer

End Extern


''' ===================================================================================================
''' seans3

Const RD_OK As Integer = 0
Const RD_ERR_FILE  As Integer = 1
Const RD_ERR_MEM  As Integer = 2

Type  seansRd_data Field = 0

	Dim Day1		As Integer
	Dim Month1	As Integer
	Dim Year1	As Integer
	Dim Hour1	As Integer
	Dim Minute1	As Integer
	Dim Second1	As Integer

	Dim NR		As Integer ' количество развёрток
	Dim NP		As Integer ' количество точек в развёртке
	Dim NC		As Integer ' количество каналов

	Dim Lag		As Double ' задержка
	Dim Freq		As Double ' частота

	Dim Dat1		As Integer Ptr
	Dim Dat2		As Integer Ptr
	Dim Dat3		As Integer Ptr
	Dim Dat4		As Integer Ptr

End Type

Extern "c"

Declare Function seansRd_load Alias "seansRd_load" ( ByVal filename As ZString Ptr, ByVal seans As seansRd_data Ptr) As Integer
Declare Function seansRd_close Alias "seansRd_close" (ByVal seans As seansRd_data Ptr) As Integer

End Extern


''' ===================================================================================================
''' seans3

Const R3C3_OK As Integer = 0
Const R3C3_ERR_FILE  As Integer = 1
Const R3C3_ERR_MEM  As Integer = 2
Const R3C3_ERR_TYPE  As Integer = 3

Type  seans3c3_data Field = 0


	Dim NP		As Integer ' количество точек в развёртке
	Dim NR		As Integer ' количество развёрток
	Dim Lag		As Double ' задержка

	Dim tDateTime	As Double Ptr

	Dim Dat1		As Short Ptr
	Dim Dat2		As Short Ptr
	Dim Dat3		As Short Ptr


End Type

Extern "c"

Declare Function seans3c3_load Alias "seans3c3_load" ( ByVal filename As ZString Ptr, ByVal seans As seans3c3_data Ptr) As Integer
'Declare Function seansRd_close Alias "seansRd_close" (ByVal seans As seansRd_data Ptr) As Integer

End Extern



''' ===================================================================================================
''' seansH

Type  seansH_data Field = 0

	Dim Day1								As Integer
	Dim Month1							As Integer
	Dim Year1							As Integer
	Dim Hour1							As Integer
	Dim Minute1							As Integer
	Dim Second1							As Integer

	Dim nH								As Integer
	Dim nP								As Integer
	Dim nR								As Integer

	Dim dT								As Double
	Dim dH								As Double

	Dim magic							As Short

	Dim Dat1(0 To 679, 0 To 113)	As LongInt
	Dim Dat2(0 To 679, 0 To 113)	As LongInt
	Dim Dat3(0 To 679, 0 To 113)	As LongInt
	Dim Dat4(0 To 679, 0 To 113)	As LongInt

	Dim Dat01(0 To 679)				As LongInt
	Dim Dat02(0 To 679)				As LongInt

	Dim Datps1(0 To 679)				As LongInt
	Dim Datps2(0 To 679)				As LongInt

	Dim Dat03(0 To 679)				As LongInt
	Dim Dat04(0 To 679)				As LongInt

End Type

Extern "c"


Declare Function  seansH_load Alias "seansH_load" ( _
ByVal filename As ZString Ptr, _
ByVal seans As seansH_data Ptr _
) As Integer

End Extern

''' ===================================================================================================

Type seansIV_scan Field = 0
	Dim type1 As Byte
	Dim data1 As Short Ptr
	Dim data2 As Short Ptr
End Type

Type seansIV_data Field = 0

	Dim ver								As Integer
	Dim date1							As ZString*20
	Dim nR								As Integer
	Dim nP								As Integer
	Dim dT								As Integer

	Dim scans							As seansIV_scan Ptr

End Type

Extern "c"

Declare Function  seansIV_load Alias "seansIV_load" ( _
					ByVal filename As ZString Ptr, _
					ByVal seans As seansIV_data Ptr _
					) As Integer

Declare Function  seansIV_close Alias "seansIV_close" ( _
					ByVal seans As seansIV_data Ptr _
					) As Integer

End Extern

''' ===================================================================================================
''' filters

Extern "c"

Declare Function  razr_load Alias "razr_load" (ByVal filename As ZString Ptr, ByVal array As Double Ptr, ByVal length As Integer) As Integer
Declare Function  filter_load Alias "filter_load" (ByVal filtername As ZString Ptr, ByVal array As Double Ptr, ByVal length As Integer) As Integer
Declare Function  filter_freq_resp Alias "filter_freq_resp" (ByVal filtername As ZString Ptr, ByVal freq As Double) As Double

End Extern

''' ===================================================================================================
''' myfiles

Extern "c"

Declare Function filelist_get Alias "filelist_get"  (ByVal directory As ZString Ptr, ByVal filelist As ZString Ptr) as Integer

Declare Function filelist_get_filename Alias "filelist_get_filename"  (ByVal filelist As ZString Ptr, ByVal filename As ZString Ptr, ByVal num As Integer) as Integer

Declare Function file_creat Alias "file_creat"  (ByVal filename As ZString Ptr) As Integer

Declare Function file_creat_and_add_s Alias "file_creat_and_add_s"  (ByVal filename As ZString Ptr, ByVal zstr As ZString Ptr) As Integer

Declare Function file_newline Alias "file_newline"  (ByVal filename As ZString Ptr) As Integer

Declare Function file_add_d Alias "file_add_d"  (ByVal filename As ZString Ptr, ByVal array As Double Ptr, ByVal length As Integer, ByVal out_format As ZString Ptr) As Integer

Declare Function file_add_i Alias "file_add_i"  (ByVal filename As ZString Ptr, ByVal array As Integer Ptr, ByVal length As Integer, ByVal out_format As ZString Ptr) As Integer

Declare Function file_add_s Alias "file_add_s"  (ByVal filename As ZString Ptr, ByVal array As ZString Ptr, ByVal out_format As ZString Ptr) As Integer

Declare Function file_add_bin_d Alias "file_add_bin_d"  (ByVal filename As ZString Ptr, ByVal array As Double Ptr, ByVal length As Integer) As Integer

Declare Function file_size Alias "file_size"  (ByVal filename As ZString Ptr) As Integer

Declare Function file_load_all_d Alias "file_load_all_d"  (ByVal filename As ZString Ptr, ByVal array As Double Ptr) As Integer

Declare Function buffer_clear Alias "buffer_clear"  (ByVal buf As ZString Ptr) As Integer

Declare Function buffer_newline Alias "buffer_newline"  (ByVal buf As ZString Ptr) As Integer

Declare Function buffer_add_d Alias "buffer_add_d"  (ByVal buf As ZString Ptr, ByVal array As Double Ptr, ByVal length As Integer, ByVal out_format As ZString Ptr) As Integer

Declare Function buffer_add_i Alias "buffer_add_i"  (ByVal buf As ZString Ptr, ByVal array As Integer Ptr, ByVal length As Integer, ByVal out_format As ZString Ptr) As Integer

Declare Function buffer_add_s Alias "buffer_add_s"  (ByVal buf As ZString Ptr, ByVal array As ZString Ptr, ByVal out_format As ZString Ptr) As Integer

Declare Function file_table_load Alias "file_table_load"  (ByVal fname As ZString Ptr, ByVal c As Integer Ptr, ByVal r As Integer Ptr, ByVal arr As Double Ptr) As Integer

End Extern

''' ===================================================================================================
''' mytime

Extern "c"

Declare Function date_2unixtime Alias "date_2unixtime"  (ByVal day1 As Integer, ByVal month1 As Integer, ByVal year1 As Integer, ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As UInteger
Declare Function unixtime_2date Alias "unixtime_2date"  (ByVal timestamp As UInteger, ByVal day1 As Integer Ptr, ByVal month1 As Integer Ptr, ByVal year1 As Integer Ptr, ByVal hh As Integer Ptr, ByVal mm As Integer Ptr, ByVal ss As Integer Ptr) As Integer
Declare Function time_2decimal Alias "time_2decimal"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As Double
Declare Function time_2str Alias "time_2str"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer, ByVal timestr As ZString Ptr) As Integer
Declare Function date_2str Alias "date_2str"  (ByVal dd As Integer, ByVal mm As Integer, ByVal yy As Integer, ByVal datestr As ZString Ptr) As Integer
Declare Function time_2sec Alias "time_2sec"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As Integer
Declare Function time_accum_num Alias "time_accum_num"  (ByVal minutes As Integer) As Integer
Declare Function date_year_leap Alias "date_year_leap" (ByVal year1 As Integer) As Integer
Declare Function date_next Alias "date_next" (ByVal day1 As Integer Ptr, ByVal month1 As Integer Ptr, ByVal year1 As Integer Ptr) As Integer
Declare Function date_valid Alias "date_valid" (ByVal day1 As Integer, ByVal month1 As Integer, ByVal year1 As Integer) As Integer
Declare Function time_from_str Alias "time_from_str"  (ByVal hh As Integer Ptr, ByVal mm As Integer Ptr, ByVal ss As Integer Ptr,  ByVal timestr As ZString Ptr) As Integer
Declare Function date_from_str Alias "date_from_str"  (ByVal dd As Integer Ptr, ByVal mm As Integer Ptr, ByVal yy As Integer Ptr,  ByVal datestr As ZString Ptr) As Integer
Declare Function tdatetime_2date Alias "tdatetime_2date"  (ByVal tdatetime As Double, ByVal day1 As Integer Ptr, ByVal month1 As Integer Ptr, ByVal year1 As Integer Ptr, ByVal hh As Integer Ptr, ByVal mm As Integer Ptr, ByVal ss As Integer Ptr, ByVal ms As Integer Ptr) As Integer
Declare Function time_linear Alias "time_linear" (ByVal arr As Double Ptr, ByVal length As Integer) As Integer
Declare Function time_normalize Alias "time_normalize" (ByVal arr As Double Ptr, ByVal length As Integer) As Integer

End Extern

''' ===================================================================================================
''' mymath

Extern "c"

Declare Function stat_mean_d Alias "stat_mean_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_deviation_d Alias "stat_deviation_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_variance_d Alias "stat_variance_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_distribution_d Alias "stat_distribution_d"  (ByVal in1 As Double Ptr, ByVal in_len As Integer, ByVal out1 As Double Ptr, ByVal out_len As Integer, ByVal start1 As Double, ByVal step1 As Double) As Integer

Declare Function fourier_DFT_d Alias "fourier_DFT_d"  (ByVal array_in As Double Ptr, ByVal il As Integer, ByVal array_out As Double Ptr, ByVal ol As Integer, ByVal f0 As Double, ByVal df As Double, ByVal dt As Double) As Integer

Declare Function fourier_DCT_d Alias "fourier_DCT_d"  (ByVal array_in As Double Ptr, ByVal il As Integer, ByVal array_out As Double Ptr, ByVal ol As Integer, ByVal f0 As Double, ByVal df As Double, ByVal dt As Double) As Integer

Declare Function fourier_FFT_d Alias "fourier_FFT_d"  (ByVal array_in1 As Double Ptr, ByVal array_in2 As Double Ptr, ByVal array_out1 As Double Ptr, ByVal array_out2 As Double Ptr, ByVal length As Integer) As Integer

Declare Function fourier_IFFT_d Alias "fourier_IFFT_d"  (ByVal array_in1 As Double Ptr, ByVal array_in2 As Double Ptr, ByVal array_out1 As Double Ptr, ByVal array_out2 As Double Ptr, ByVal length As Integer) As Integer

Declare Function fourier_get_spectrum_from_acf Alias "fourier_get_spectrum_from_acf"  (ByVal c As Double Ptr, ByVal s As Double Ptr, ByVal length As Integer, ByVal deltaTau As Double, ByVal sp As Double Ptr, ByVal nF As Integer) As Integer

Declare Function fourier_get_acf_from_spectrum Alias "fourier_get_acf_from_spectrum"  (ByVal sp As Double Ptr, ByVal nF As Integer, ByVal c As Double Ptr, ByVal s As Double Ptr, ByVal length As Integer, ByVal deltaTau As Double) As Integer

Declare Function array_max_d Alias "array_max_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function array_min_d Alias "array_min_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function array_add_d Alias "array_add_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function array_sub_d Alias "array_sub_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function array_mul_d Alias "array_mul_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function array_div_d Alias "array_div_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function array_norm_d Alias "array_norm_d"  (ByVal in1 As Double Ptr, ByVal out1 As Double Ptr, ByVal num As Double, ByVal length As Integer) As Integer

Declare Function array_norm0_d Alias "array_norm0_d"  (ByVal in1 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function array_error_d Alias "array_error_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal length As Integer) As Double

Declare Function array_linear_d Alias "array_linear_d"  (ByVal x As Double, ByVal in_x As Double Ptr, ByVal in_y As Double Ptr, ByVal length As Integer) As Double

Declare Function array_trand_d Alias "array_trand_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal num As Integer, ByVal length As Integer) As Integer

Declare Function func_conv_d Alias "func_conv_d"  (ByVal in1 As Double Ptr, ByVal in2 As Double Ptr, ByVal out1 As Double Ptr, ByVal length As Integer) As Integer

Declare Function spline_bspline3_d Alias "spline_bspline3_d"  (ByVal in_x As Double Ptr, ByVal in_y As Double Ptr, ByVal in_len As Integer, ByVal out_x As Double Ptr, ByVal out_y As Double Ptr, ByVal length As Integer) As Integer

Declare Function approx_poly_coeff_d Alias "approx_poly_coeff_d"  (ByVal in_x As Double Ptr, ByVal in_y As Double Ptr, ByVal in_len As Integer, ByVal out_coeff As Double Ptr, ByVal N As Integer) As Integer

Declare Function random_randomize Alias "random_randomize"  (ByVal num As Integer) As Integer

Declare Function random_rdtsc Alias "random_rdtsc"  () as Integer

Declare Function random_rnd Alias "random_rnd"  () as Double

Declare Function random_rnd_int Alias "random_rnd_int"  (n As Integer) as Integer

Declare Function random_gauss Alias "random_gauss"  () as Double

End Extern

''' ===================================================================================================
''' pr_z

Extern "c"

Declare Function spectrum_1 Alias "spectrum_1" ( ByVal m1 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double
Declare Function spectrum_2 Alias "spectrum_2" ( ByVal m1 As Double, ByVal m2 As Double, ByVal g1 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double
Declare Function spectrum_3 Alias "spectrum_3" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double

Declare Function acf_1  Alias "acf_1" ( ByVal m1 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As Integer
Declare Function acf_2  Alias "acf_2" ( ByVal m1 As Double, ByVal m2 As Double, ByVal g1 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As Integer
Declare Function acf_3  Alias "acf_3" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer

Declare Function acf_3_full Alias "acf_3_full" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal acf As Double Ptr, ByVal length As Integer, ByVal dt As Double) As  Integer
Declare Function spectrum_3_full Alias "spectrum_3_full" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal freq As Double) As Double

Declare Function acf_3_full_millstone Alias "acf_3_full_millstone" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal acf As Double Ptr, ByVal length As Integer, ByVal dt As Double) As  Integer
Declare Function spectrum_3_full_millstone Alias "spectrum_3_full_millstone" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal freq As Double) As Double

Declare Function acf_library_light_short       Alias "acf_library_light_short"      (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer
Declare Function acf_library_light_short_conv  Alias "acf_library_light_short_conv" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer
Declare Function library_light_list_of_temperatures_get  Alias "library_light_list_of_temperatures_get" (ByVal list As Integer Ptr ) As Integer
Declare Function library_light_list_of_temperatures_get2  Alias "library_light_list_of_temperatures_get2" (ByVal list As Integer Ptr ) As Integer
Declare Function library_light_list_get  Alias "library_light_list_get" (ByVal prefix As ZString Ptr, ByVal he_percent_int As Integer, ByVal list As ZString Ptr ) As  Integer
Declare Function library_light_list_get_filename  Alias "library_light_list_get_filename" (ByVal filename As ZString Ptr, ByVal filelist As ZString Ptr, ByVal num As Integer) As  Integer

Declare Function library_heavy_list_of_temperatures_get  Alias "library_heavy_list_of_temperatures_get" (ByVal list As Integer Ptr ) As Integer
Declare Function acf_library_heavy Alias "acf_library_heavy" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer

Declare Function library_oxygen_list_of_temperatures_get  Alias "library_oxygen_list_of_temperatures_get" (ByVal list As Integer Ptr ) As Integer
Declare Function acf_library_oxygen Alias "acf_library_oxygen" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer

Declare Function library_millstone_list_of_temperatures_get  Alias "library_millstone_list_of_temperatures_get" (ByVal list As Integer Ptr ) As Integer
Declare Function acf_library_millstone Alias "acf_library_millstone" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Single Ptr, ByVal length As Integer) As  Integer

Declare Function acf_3_kharkiv_22 Alias "acf_3_kharkiv_22" (ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr) As  Integer

End Extern

''' ===================================================================================================
''' mylist

Type list Field = 1
	Dim filename		As ZString Ptr
	Dim time_computer	As Integer
	Dim Data1			As Any Ptr
	Dim prev_list		As list Ptr
	Dim next_list		As list Ptr
End Type

Extern "c"

Declare Function list_add Alias "list_add" ( ByVal top As list Ptr Ptr, ByVal filename As ZString Ptr, ByVal time_computer As Integer, ByVal data As Any Ptr, ByVal size As Integer) As  Integer

Declare Function list_get_filename_num Alias "list_get_filename_num" ( ByVal top As list Ptr Ptr, ByVal filename As ZString Ptr, ByVal num As Integer) As  Integer

Declare Function list_get_time_num Alias "list_get_time_num" (  ByVal top As list Ptr Ptr, ByVal time_computer As Integer Ptr, ByVal num As Integer) As  Integer

Declare Function list_get_data_num Alias "list_get_data_num" (  ByVal top As list Ptr Ptr, ByVal data As Any Ptr, ByVal size As Integer, ByVal num As Integer) As  Integer

Declare Function list_insert Alias "list_insert" ( ByVal top As list Ptr Ptr, ByVal filename As ZString Ptr, ByVal time_computer As Integer, ByVal data As Any Ptr, ByVal size As Integer, ByVal num As Integer) As  Integer

Declare Function list_len Alias "list_len" ( ByVal top As list Ptr Ptr)  As  Integer

End Extern



''' ===================================================================================================
' Работа с ионограммами

Type ionogram_data Field = 0

	Dim Day1							As Integer
	Dim Month1						As Integer
	Dim Year1						As Integer
	Dim Hour1						As Integer
	Dim Minute1						As Integer
	Dim Second1						As Integer

	Dim nr							As Integer
	Dim nf							As Integer
	Dim nh							As Integer

	Dim Dat(0 To 15, 0 To 399, 0 To 249) As UByte

End Type


Extern "c"

Declare Function  ionogram_load Alias "ionogram_load" ( _
ByVal filename As ZString Ptr, _
ByVal ionogram As ionogram_data Ptr _
) As Integer

End Extern

''' ===================================================================================================
