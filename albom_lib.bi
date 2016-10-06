
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
' ���������� albom.dll

#Inclib "./albom"

''' ===================================================================================================
' �������� scan-����� ������ � ���������� ������

Enum AlbomKeys
	KEY_CTRL_A = 1 
	KEY_BACKSPACE = 8
	KEY_TAB = 9
	KEY_ENTER = 13
	KEY_CTRL_N = 14
	KEY_CTRL_Q = 17
	KEY_ESC = 27
	KEY_SPACE = 32
	KEY_PLUS = 43
	KEY_MINUS = 45
	KEY_1 = 49
	KEY_2 = 50
	KEY_3 = 51
	KEY_4 = 52
	KEY_A = 97
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
	KEY_Y = 121
	KEY_F1 = 15359
	KEY_LEFT = 19455
	KEY_RIGHT = 19967 
	KEY_UP = 18687
	KEY_DOWN = 20735
	KEY_DEL = 21503
	KEY_CTRL_LEFT = 29695
	KEY_CTRL_RIGHT = 29951
	KEY_CTRL_UP = 36351
	KEY_CTRL_DOWN = 37375
	KEY_SHIFT_LEFT = 19455
	KEY_SHIFT_RIGHT = 19967
	KEY_PAGE_UP = 18943
	KEY_PAGE_DOWN = 20991
	KEY_HOME = 18431
	KEY_END = 20479 
End Enum



''' ===================================================================================================
' ������� Print_process - ������� ����������� ���������


Function Print_process(ByVal num As Integer) As Integer
	Select Case num
		Case 0: Print "|"; : Locate ,Pos-1: Return num+1
		Case 1: Print "/"; : Locate ,Pos-1: Return num+1
		Case 2: Print "-"; : Locate ,Pos-1: Return num+1
		Case 3: Print "\"; : Locate ,Pos-1: Return 0
	End Select
End Function

''' ===================================================================================================
' ������� Print_process - ������� ���������� ���������

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
' ��������� break - ������� ������� ������� � ��������� ������ ���������

Sub break()
	Sleep 
	End
End Sub



''''' ===================================================================================================
' �-��� ���������� 1, ���� ������ Y
' ��� 0, ���� ������ N

Function GetKey_YN() As Integer
	Dim key As Integer
	Do
		key = GetKey
	Loop Until key = KEY_Y Or key = KEY_N
	
	If key = KEY_Y Then Print "Y": Return 1 Else Print "N": Return 0
End Function



''' ===================================================================================================
' ��������� ������ � ����� S-������, � ����� V- � C- ������

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

	Dim m(0 To 179)				As Integer 
	
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

	Dim m(0 To 269)				As Integer 
	
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

Declare Function  seans1s_saveM0 Alias "seans1s_saveM0" (_
											 ByVal filename As ZString Ptr _
											 ) As Integer

Declare Function  seans1s_saveM3 Alias "seans1s_saveM3" (_
											 ByVal filename As ZString Ptr, _
											 ByVal seans As seans1s_data Ptr _
										) As Integer

Declare Function  seans1s_alt Alias "seans1s_alt" (ByVal H As Integer) As Double

Declare Function  seans1s_noise Alias "seans1s_noise" (_
											 ByVal seans As seans1s_data Ptr, _
											 ByVal acf_noise As Double Ptr, _
											 ByVal length As Integer, _ 
											 ByVal alt_start As Integer, _
											 ByVal alt_end As Integer _
										) As Integer

Declare Function  seans1c_test Alias "seans1c_test" ( ByVal filename As ZString Ptr) As Integer

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

Declare Function  seans3_load Alias "seans3_load" ( ByVal filename As ZString Ptr, ByVal seans As seans3_data Ptr) As Integer

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

Declare Function file_creat Alias "file_creat"  (ByVal filename As String) As Integer

Declare Function file_newline Alias "file_newline"  (ByVal filename As String) As Integer

Declare Function file_add_d Alias "file_add_d"  (ByVal filename As String, ByVal array As Double Ptr, ByVal length As Integer, ByVal out_format As String) As Integer

Declare Function file_add_i Alias "file_add_i"  (ByVal filename As String, ByVal array As Integer Ptr, ByVal length As Integer, ByVal out_format As String) As Integer

Declare Function file_add_s Alias "file_add_s"  (ByVal filename As String, ByVal array As String Ptr, ByVal out_format As String) As Integer

Declare Function file_add_bin_d Alias "file_add_bin_d"  (ByVal filename As String, ByVal array As Double Ptr, ByVal length As Integer) As Integer

Declare Function file_size Alias "file_size"  (ByVal filename As String) As Integer

Declare Function file_load_all_d Alias "file_load_all_d"  (ByVal filename As String, ByVal array As Double Ptr) As Integer

Declare Function buffer_clear Alias "buffer_clear"  (ByVal buf As String Ptr) As Integer

Declare Function buffer_newline Alias "buffer_newline"  (ByVal buf As String Ptr) As Integer

Declare Function buffer_add_d Alias "buffer_add_d"  (ByVal buf As String Ptr, ByVal array As Double Ptr, ByVal length As Integer, ByVal out_format As String) As Integer

Declare Function buffer_add_i Alias "buffer_add_i"  (ByVal buf As String Ptr, ByVal array As Integer Ptr, ByVal length As Integer, ByVal out_format As String) As Integer

Declare Function buffer_add_s Alias "buffer_add_s"  (ByVal buf As String Ptr, ByVal array As String, ByVal out_format As String) As Integer

End Extern

''' ===================================================================================================
''' mytime

Extern "c"

Declare Function date_2unixtime Alias "date_2unixtime"  (ByVal day1 As Integer, ByVal month1 As Integer, ByVal year1 As Integer, ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As Integer
Declare Function unixtime_2date Alias "unixtime_2date"  (ByVal timestamp As Integer, ByVal day1 As Integer Ptr, ByVal month1 As Integer Ptr, ByVal year1 As Integer Ptr, ByVal hh As Integer Ptr, ByVal mm As Integer Ptr, ByVal ss As Integer Ptr) As Integer
Declare Function time_2decimal Alias "time_2decimal"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As Double
Declare Function time_2str Alias "time_2str"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer, ByVal timestr As ZString Ptr) As Integer
Declare Function date_2str Alias "date_2str"  (ByVal dd As Integer, ByVal mm As Integer, ByVal yy As Integer, ByVal datestr As ZString Ptr) As Integer
Declare Function time_2sec Alias "time_2sec"  (ByVal hh As Integer, ByVal mm As Integer, ByVal ss As Integer) As Integer
Declare Function time_accum_num Alias "time_accum_num"  (ByVal minutes As Integer) As Integer
Declare Function date_year_leap Alias "date_year_leap" (ByVal year1 As Integer) As Integer
Declare Function date_next Alias "date_next" (ByVal day1 As Integer Ptr, ByVal month1 As Integer Ptr, ByVal year1 As Integer Ptr) As Integer
Declare Function date_valid Alias "date_valid" (ByVal day1 As Integer, ByVal month1 As Integer, ByVal year1 As Integer) As Integer
Declare Function time_from_str Alias "time_from_str"  (ByVal hh As Integer Ptr, ByVal mm As Integer Ptr, ByVal ss As Integer Ptr,  ByVal timestr As ZString Ptr) As Integer

End Extern

''' ===================================================================================================
''' mymath

Extern "c"

Declare Function stat_mean_d Alias "stat_mean_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_deviation_d Alias "stat_deviation_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_variance_d Alias "stat_variance_d"  (ByVal array As Double Ptr, ByVal length As Integer) as Double

Declare Function stat_distribution_d Alias "stat_distribution_d"  (ByVal in1 As Double Ptr, ByVal in_len As Integer, ByVal out1 As Double Ptr, ByVal out_len As Integer, ByVal start1 As Double, ByVal step1 As Double) As Integer

Declare Function fourier_DFT_d Alias "fourier_DFT_d"  (ByVal array_in As Double Ptr, ByVal il As Integer, ByVal array_out As Double Ptr, ByVal ol As Integer, ByVal f0 As Double, ByVal df As Double, ByVal dt As Double) As Integer

Declare Function fourier_DCFT_d Alias "fourier_DCFT_d"  (ByVal array_in As Double Ptr, ByVal il As Integer, ByVal array_out As Double Ptr, ByVal ol As Integer, ByVal f0 As Double, ByVal df As Double, ByVal dt As Double) As Integer

Declare Function fourier_FFT_d Alias "fourier_FFT_d"  (ByVal array_in1 As Double Ptr, ByVal array_in2 As Double Ptr, ByVal array_out1 As Double Ptr, ByVal array_out2 As Double Ptr, ByVal len As Integer) As Integer

Declare Function fourier_IFFT_d Alias "fourier_IFFT_d"  (ByVal array_in1 As Double Ptr, ByVal array_in2 As Double Ptr, ByVal array_out1 As Double Ptr, ByVal array_out2 As Double Ptr, ByVal len As Integer) As Integer

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

Declare Function random_gauss Alias "random_gauss"  () as Double

End Extern

''' ===================================================================================================
''' pr_z

Extern "c"

Declare Function prz_si1 Alias "prz_si1" ( ByVal m1 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double
Declare Function prz_si2 Alias "prz_si2" ( ByVal m1 As Double, ByVal m2 As Double, ByVal g1 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double
Declare Function prz_si3 Alias "prz_si3" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal freq As Double) As Double

Declare Function prz_acf1  Alias "prz_acf1" ( ByVal m1 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As Integer
Declare Function prz_acf2  Alias "prz_acf2" ( ByVal m1 As Double, ByVal m2 As Double, ByVal g1 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As Integer
Declare Function prz_acf3  Alias "prz_acf3" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer

Declare Function prz_acf3_univers Alias "prz_acf3_univers" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer, ByVal df As Double, ByVal dt As Double) As  Integer

Declare Function prz_acf3_full Alias "prz_acf3_full" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal acf As Double Ptr, ByVal length As Integer, ByVal dt As Double) As  Integer
Declare Function prz_si3_full_array Alias "prz_si3_full_array" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal w As Double Ptr, ByVal length As Integer, ByVal df As Double) As Integer
Declare Function prz_si3_full Alias "prz_si3_full" ( ByVal m1 As Double, ByVal m2 As Double, ByVal m3 As Double, ByVal g1 As Double, ByVal g2 As Double, ByVal ti As Double, ByVal te As Double, ByVal ne As Double, ByVal iskD As Integer, ByVal freq As Double) As Double


Declare Function fortran_library_list_of_temperatures_get  Alias "fortran_library_list_of_temperatures_get" (ByVal list As Integer Ptr ) As  Integer
Declare Function fortran_library_list_get  Alias "fortran_library_list_get" (ByVal he_percent_int As Integer, ByVal list As ZString Ptr ) As  Integer
Declare Function fortran_library_list_get_filename  Alias "fortran_library_list_get_filename" (ByVal filename As ZString Ptr, ByVal filelist As ZString Ptr, ByVal num As Integer) As  Integer
Declare Function fortran_library_memory_load  Alias "fortran_library_memory_load" (ByVal f As FILE Ptr, ByVal memory As Double Ptr ) As  Integer
Declare Function prz_acf_fortran  Alias "prz_acf_fortran" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer
Declare Function prz_acf_fortran_memory  Alias "prz_acf_fortran_memory" (ByVal memory As Double Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer
Declare Function fortran_library_list_get_conv  Alias "fortran_library_list_get_conv" (ByVal he_percent_int As Integer, ByVal list As ZString Ptr ) As  Integer
Declare Function prz_acf_fortran_conv  Alias "prz_acf_fortran_conv" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As  Integer

Declare Function prz_m_library_make Alias "prz_M_library_make" (ByVal filename As ZString Ptr, ByVal O As Integer) As Integer
Declare Function prz_m_library_list_of_temperatures_get  Alias "prz_M_library_list_of_temperatures_get" (ByVal list As Integer Ptr ) As Integer
Declare Function prz_m_acf Alias "prz_M_acf" (ByVal f As FILE Ptr, ByVal temperatures As Integer Ptr, ByVal temperatures_len As Integer, ByVal ti As Double, ByVal te As Double, ByVal acf As Double Ptr, ByVal length As Integer) As Integer


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
' ������ � ������������

Type ionogram_data Field = 1

	Dim Day1							As Integer 
	Dim Month1						As Integer 
	Dim Year1						As Integer 
	Dim Hour1						As Integer 
	Dim Minute1						As Integer 
	Dim Second1						As Integer 

	Dim nc							As Integer
	Dim nf							As Integer
	Dim np							As Integer   

	Dim Dat(0 To 399, 0 To 15, 0 To 249) As UByte				

End Type


Extern "c"

Declare Function  ionogram_load Alias "ionogram_load" ( _
											 ByVal filename As ZString Ptr, _
											 ByVal ionogram As ionogram_data Ptr _
										) As Integer

End Extern

''' ===================================================================================================