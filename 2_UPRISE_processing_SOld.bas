
#Include Once "albom_lib.bi"	' �������� ���������� "albom.dll"
#Include Once "albom_log.bi"		' ����������� ����
#Include Once "albom_as_file.bi"	' �������� �������� � �������� ��� ������ � AS-�������
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "dir.bi"

'''==============================================

Type seans_struct_in
	Dim seans 			As seans1s_data
	Dim time_computer	As Integer
End Type



Type seans_struct_out
	Dim datCos(0 To 18)		As Double  Ptr
'	Dim datSin(0 To 18)		As Double  Ptr
	Dim datCosTrap(0 To 18)	As Double  Ptr
'	Dim datSinTrap(0 To 18)	As Double  Ptr
	Dim m							As Integer Ptr
'	Dim pShort					As Double  Ptr
End Type



Type seans_struct_time
	Dim time_computer		As Integer Ptr
	Dim date_				As ZString Ptr Ptr
	Dim time_				As ZString Ptr Ptr
End Type


'''==============================================

Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 As Any Ptr, elem2 As Any Ptr) As Long

'''==============================================

Dim Shared As ZString Ptr lst  ' ������ ������

Dim Shared As seans_struct_in Ptr seans_str_in '������ �������� ������ �� �������
Dim Shared As Integer seans_loaded   ' ������� ���������

Dim Shared As Integer seans_out_num   ' ���������� �������� �������

Dim Shared As seans_struct_out seans_str_out(0 To 679) '������ �������� ������ �� �������
Dim Shared As seans_struct_time seans_str_time '�������� ������ ��� ������� (������� �������� �������� �� seans_str_out ��� �������� �����)

Dim As Integer i, j
Dim As Integer t, h, tau

Dim As Integer file

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_IN = "./in/"
Dim As String SEANS_DIR_OUT = "./out/"
Dim As String EXT
Dim As Integer symbol = 0 ' ��������������� ���������� ��� ����������� �������� �������� �������

Dim As Integer Day1, Month1, Year1, Hour1, Minute1, Second1

Dim As Double Ptr trand
Dim As Integer tNak
Dim As Integer tStep
Dim As Integer n2

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename

Dim Shared As Double R0DAC_1(0 To 18) ' 0 ��� 1-�� ����������� ������

Dim As as_file_struct as_file_str ' ����� ��� ��������� �����

Dim Shared noiseAcfCos(0 To 18)	As Double  Ptr

Dim As Integer partrap = 0

Dim As Integer is_divide=1

Dim Shared As Integer pulseLength


'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"



Dim As String  tmp
file = FreeFile()
Open "config/config.dat" For Input As #file
Input #file, razr_filename
Input #file, tmp
Input #file, tmp
Input #file, tmp
Input #file, pulseLength
Close #file


If razr_load(razr_filename, @razr(0), 330) = 0 Then ' �������� ����������
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf

If (pulseLength <> 663) And (pulseLength <> 795) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf


Open Err For Output As #1


Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Processing - ��������� ���������� ������ (S-������ ������� �1) � ������� �������� ������"
Print "(c) ������� �.�., ����� �.�. (�������� ���������)"
Color 8
Print
Print "================================"
Print "��������� ������� " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
Print "================================"
Print
Color 11

Print "�������� ������, ����������� � ����� " + Chr(34) + "in" + Chr(34) + ":"
Color 10
Dim As String fn
fn = Dir("./in/*", fbDirectory)
While Len(fn) > 0 
	fn = Dir()
	If Len(fn)=6 Then
		Print fn;"  ";
	EndIf
Wend
Print
Print

Color 15

Input "������� ���� ������ ��������� (���� ����� ���): ", d_day, d_month, d_year
Input "������� ���������� �����: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

Print
Input "������� ����� ���������� (� ���): ", Tnak
Input "������� ��� ����������� ���� (� ���): ", Tstep
Input "������� ���������� ����� ��� ������������: ", n2

partrap = 0

If (Tnak < 1) Or (Tstep < 1) Or (n2 < 1) Or (partrap < 0) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf


' �������� �������
Print
Print "�������� �������... ";

seans_loaded = 0
seans_str_in = NULL

lst = Allocate (2*1024*1024*SizeOf(Byte)) ' �������� �� ������ ������ 2 ������
If lst = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf


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

		LoadFiles(SEANS_DIR_IN+directory)

		date_next(@d_day, @d_month, @d_year)

		If i = 0 Then
			DirectoryOutput += directory + "-"
		EndIf

	EndIf

Next i

DeAllocate (lst)

If seans_loaded = 0 Then  ' ���� �� ���� ����� �� ��������, �������, �������������� ����� ������ � ���
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"

Print #1, Str(seans_loaded)+" files loaded"


MkDir(SEANS_DIR_OUT)

DirectoryOutput += directory
MkDir(SEANS_DIR_OUT +DirectoryOutput)
MkDir(SEANS_DIR_OUT +DirectoryOutput+"/step2")

file = FreeFile()
Open SEANS_DIR_OUT +DirectoryOutput+"/step2/input.txt" For Output As #file
Print #file, "����� ���������� (� ���): "; Tnak
Print #file, "��� ����������� ���� (� ���): "; Tstep
Print #file, "���������� ����� ��� ������������: "; n2
Print #file, "�������� ���������������� ������������: "; partrap
Close #file


' ���������� ������� �� �������
Print "���������� �� �������... ";
qsort(seans_str_in, seans_loaded, SizeOf(seans_struct_in), @seans_struct_time_compare)
Print "OK"

Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"

Print "���������� ������... ";




' ������ ���������� �������� ������
seans_out_num = 1
For t = 0 To seans_loaded-1-1 ' �� �������

	If ( seans_str_in[t+1].time_computer - seans_str_in[t].time_computer ) > 119 Then
		seans_out_num += (seans_str_in[t+1].time_computer - seans_str_in[t].time_computer)/60
	Else
		seans_out_num += 1
	EndIf

Next t




'�������� ������ �� �������� ������

For h = 0 To 679 ' �� ������

	For tau = 0 To 18

		seans_str_out(h).datCos(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datCos(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

		seans_str_out(h).datCosTrap(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datCosTrap(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

	Next tau

	seans_str_out(h).m = Callocate( seans_out_num, SizeOf(Integer) )
	If seans_str_out(h).m = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

Next h






'�������� ������ �� �����
seans_str_time.time_computer = Callocate( seans_out_num, SizeOf(Integer) )
If seans_str_time.time_computer = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

'�������� ������ �� ������� ����� ��� ���� � �������
seans_str_time.date_ = Callocate( seans_out_num, SizeOf(ZString Ptr) )
If seans_str_time.date_ = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

seans_str_time.time_ = Callocate( seans_out_num, SizeOf(ZString Ptr) )
If seans_str_time.time_ = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

For i = 0 To seans_out_num-1 '�������� ������ �� ������ ��� ���� � �������
	seans_str_time.date_[i] = Callocate(16, 1)
	If seans_str_time.date_[i] = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	seans_str_time.time_[i] = Callocate(16, 1)
	If seans_str_time.time_[i] = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
Next i



' ������������ ��� ������� �� + ����

' ������������ ��������� ������ � �������� �������
i = 0 ' ������� � �������� �������
For t = 0 To seans_loaded-1-1 ' �� �������

	date_2str(  seans_str_in[t].seans.day1,_
	seans_str_in[t].seans.month1, _
	seans_str_in[t].seans.year1, _
	seans_str_time.date_[i] )

	time_2str(  seans_str_in[t].seans.hour1, _
	seans_str_in[t].seans.minute1, _
	seans_str_in[t].seans.second1, _
	seans_str_time.time_[i] )

	seans_str_time.time_computer[i] = seans_str_in[t].time_computer

	For h = 0 To 679 ' �� �������
		If seans_str_in[t].seans.m(h) = 0 Then
			seans_str_out(h).m[i] = 1

			If h < 679-19 Then
				For tau = 0 To 18
					R0DAC_1(tau) = CDbl(seans_str_in[t].seans.datm(h))*CDbl(seans_str_in[t].seans.datm(h+tau))/1463.0
				Next tau
			Else
				For tau = 0 To 18
					R0DAC_1(tau) = 0
				Next tau
			EndIf

			seans_str_out(h).datCos(0)[i] = CDbl(seans_str_in[t].seans.datp(h)) - R0DAC_1(0)

			If h Mod 4 = 0 Then
				For tau = 1 To 18 ' �� ��������
					seans_str_out(h).datCos(tau)[i] = CDbl(seans_str_in[t].seans.dat(h\4, tau-1)) - R0DAC_1(tau) 
				Next tau
			EndIf

		EndIf
	Next h



	For h = 4 To 679-4 ' �� �������

		If h Mod 4 <> 0 Then

			Dim As Integer h0, h1

			h0 = (h \ 4)*4
			h1 = h0 + 4

			For tau = 1 To 18 ' �� ��������
				seans_str_out(h).datCos(tau)[i] = seans_str_out(h0).datCos(tau)[i] + (seans_str_out(h1).datCos(tau)[i]-seans_str_out(h0).datCos(tau)[i])/(h1-h0)*(h-h0)
			Next tau

		EndIf

	Next h

	If ( seans_str_in[t+1].time_computer - seans_str_in[t].time_computer ) > 119 Then
		i += (seans_str_in[t+1].time_computer - seans_str_in[t].time_computer)/60
	Else
		i += 1
	EndIf

Next t

'������������ ������, ������� �������� ��������

DeAllocate(seans_str_in)





' ���������� ������� � ����������� ������
For t = 0 To seans_out_num-1 ' �� �������
	If seans_str_time.time_computer[t] = 0 Then
		i = 0
		Do Until ( (seans_str_time.time_computer[t + 1 + i] <> 0) Or (t + i + 1  > seans_out_num) )
			i += 1
		Loop
		seans_str_time.time_computer[t] = seans_str_time.time_computer[t-1] + (seans_str_time.time_computer[t+1+i] - seans_str_time.time_computer[t-1] )/( i+2)
		unixtime_2date(seans_str_time.time_computer[t], @Day1, @Month1, @Year1, @Hour1, @Minute1, @Second1)
		date_2str( Day1, Month1, Year1, seans_str_time.date_[t])
		time_2str( Hour1, Minute1, Second1, seans_str_time.time_[t])
	EndIf
Next t

Print "OK"


Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"



Print "������������... ";


For h = 0 To 679 ' �� ���� �������

	For tau = 0 To 18 ' �� ���� ���������


		' 1) ��� ����������� ������

		For t = 0 To seans_out_num-1 ' �� �������

			' ���� ���� �������
			If seans_str_out(h).m[t] <> 1 Then

				Dim As Integer c = 0
				Dim As Double d = 0

				' �����
				i = 0
				Do While (c < n2\2)

					If t-i < 0 Then
						Exit Do
					EndIf

					If seans_str_out(h).m[t-i] = 1 Then
						c += 1
						d += seans_str_out(h).datCos(tau)[t-i]
					EndIf

					i += 1

				Loop

				' ������
				i = 0
				Do While (c < n2\2)

					If t+i > seans_out_num-1 Then
						Exit Do
					EndIf

					If seans_str_out(h).m[t+i] = 1 Then
						c += 1
						d += seans_str_out(h).datCos(tau)[t+i]
					EndIf

					i += 1

				Loop

				If c <> 0 Then
					seans_str_out(h).datCos(tau)[t] = d/c
				Else
					seans_str_out(h).datCos(tau)[t] = 0
				EndIf

			EndIf

		Next t

	Next tau

Next h

Print "OK"


Print "��������� ����... ";


'�������� ������ �� ��� ���� ��� ������� ������
For tau = 0 To 18
	noiseAcfCos(tau) = Callocate( seans_out_num, SizeOf(Double) )
	If noiseAcfCos(tau) = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
Next tau



For t = 0 To seans_out_num-1

	' ���������� ��� ����
	For tau = 0 To 18 ' �� ��������
		noiseAcfCos(tau)[t] = 0
		For h = 500 To 599 ' �� ������
			noiseAcfCos(tau)[t] += seans_str_out(h).datCos(tau)[t]
		Next h
		noiseAcfCos(tau)[t] /= 100
	Next tau


	' ��������� ����
	For h = 0 To 679
		For tau = 0 To 18
			seans_str_out(h).datCos(tau)[t] -= noiseAcfCos(tau)[t]
		Next tau
	Next h

Next t

Print "OK"



Print "���� �������������� ����������... ";

For t = 0 To seans_out_num-1


	' ���� ����������
	For h = 0 To 679 ' �� ������
		' ��������������� ��������� ���������� (����� ������ � �����)
		Dim As Integer l1, l2 ' �������
		Dim As Double  r1, r2 ' �������� ������������ ��������

		l1 = h
		For tau = 0 To 18 ' �� ��������

			l2 = h+tau

			If l1 < 300 Then
				r1 = razr(l1)
			Else
				r1 = 1.0
			EndIf

			If l2 < 300 Then
				r2 = razr(l2)
			Else
				r2 = 1.0
			EndIf

			If r1*r2 > 1e-6 Then
				seans_str_out(h).datCos(tau)[t] /= Sqr( r1*r2 )
			Else
				seans_str_out(h).datCos(tau)[t] = 0
			EndIf

		Next tau

	Next h

Next t



Print "OK"





Print "��������������� ������������... ";


For t = 0 To seans_out_num-1

	Print_process_percent((t*100)/seans_out_num)

	' 1) ��� ���������� ������������

	For h = 0 To 679

		If h >= 18+partrap And h <= 679-partrap Then

			For tau = 0 To 18

				seans_str_out(h).datCosTrap(tau)[t] = 0
				For z As Integer = h-tau-partrap To h+partrap ' �� ������
					seans_str_out(h).datCosTrap(tau)[t] += seans_str_out(z).datCos(tau)[t]
				Next z

				If is_divide = 1 Then
					seans_str_out(h).datCosTrap(tau)[t]  /= tau+2*partrap+1 ' ������ �� ���������� ���������
				EndIf

			Next tau

		Else

			For tau = 0 To 18
				seans_str_out(h).datCosTrap(tau)[t]  = 0
			Next tau

		EndIf

	Next h

Next t

Print_process_percent(1000)
Print "OK"




Print "���������� �� �������... ";



as_file_str.acf = Callocate( 680, SizeOf(acf_struct) )   ' ����������� ������ ��� ���
If as_file_str.acf = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"

Dim As Integer seans_current = 0
t = 0
Do Until t + tNak > seans_out_num-1

	Print_process_percent((t*100)/seans_out_num)

	EXT = Str(seans_current+1)
	If seans_current+1 < 1000 Then EXT = "0" + EXT
	If seans_current+1 < 100  Then EXT = "0" + EXT
	If seans_current+1 < 10   Then EXT = "0" + EXT


	as_file_str.filename = "AS" + DirectoryOutput + "." + EXT
	as_file_str.date_ = *seans_str_time.date_[t+tNak\2]
	as_file_str.time_ = *seans_str_time.time_[t+tNak\2]

	as_file_str.nseans = seans_current+1
	as_file_str.tnak = tNak

	as_file_str.fkr = 0.0

	as_file_str.nh = 680



	' ��������� ������ �� �������
	For h = 0 To 679 ' �� ������
		as_file_str.acf[h].n = h
		If pulseLength = 663 Then
			as_file_str.acf[h].h = seans1s_alt(h)
		Else
			as_file_str.acf[h].h = seans1s_alt_795(h)
		EndIf

		For tau = 0 To 18 ' �� ��������
			as_file_str.acf[h].rc(tau) = 0
			as_file_str.acf[h].rs(tau) = 0
			For i = 0 To tNak-1
				as_file_str.acf[h].rc(tau) += seans_str_out(h).datCosTrap(tau)[t+i]
			Next i
			as_file_str.acf[h].rc(tau) /= tNak
		Next tau

	Next h



	' ��������� ��� ����
	For tau = 0 To 18 ' �� ��������
		as_file_str.rnc(tau) = 0
		as_file_str.rns(tau) = 0
		For i = 0 To tNak-1
			as_file_str.rnc(tau) += noiseAcfCos(tau)[t+i]
		Next i
		as_file_str.rnc(tau) /= tNak
	Next tau



	' ��������� ������/���
	For h = 0 To 679 ' �� ������
		If as_file_str.rnc(0) <> 0 Then
			as_file_str.acf[h].q = as_file_str.acf[h].rc(0)/as_file_str.rnc(0)
		Else
			as_file_str.acf[h].q = 0
		EndIf
	Next h

	' ������ ��������� ����� ���

	For h = 0 To 679
		For tau = 0 To 18 ' �� ��������

			as_file_str.acf[h].var(tau) = 0
			For i = 0 To tNak-1
				as_file_str.acf[h].var(tau) += (as_file_str.acf[h].rc(tau) - seans_str_out(h).datCosTrap(tau)[t+i])^2
			Next i
			as_file_str.acf[h].var(tau) /= tNak-1

		Next tau
	Next h


	as_file_save( SEANS_DIR_OUT + DirectoryOutput + "/step2/"+ "AS" + DirectoryOutput + "." + EXT,  @as_file_str) ' ������ � ����





	t += tStep
	seans_current += 1

Loop

DeAllocate(as_file_str.acf)   ' ����������� ������ � ���


Print_process_percent(1000)
Print "OK"





Print
Print "OK"

break





'''==============================================

Sub LoadFiles(ByVal Directory As String)

	Dim As Integer lst_len  ' ����� ������ ������
	Dim As Integer seans_num' ���������� ������� � ����������

	Dim As ZString*256 filename
	Dim As Integer is1s

	Dim As Integer i
	Dim As Integer symbol = 0


	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) '�������� ��� ����� �� ������
		If seans1s_test(Directory+"/"+filename) > 0 Then
			seans_num += 1
		EndIf
	Next i

	seans_str_in = ReAllocate(seans_str_in, (seans_loaded+seans_num)*SizeOf(seans_struct_in) )
	If seans_str_in = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	For i = 0 To lst_len-1

		filelist_get_filename(lst, @filename, i)

		is1s = seans1s_test(directory + "/" + filename)

		If (is1s <> 0) Then
			seans1s_load ( directory + "/" + filename, @(seans_str_in[seans_loaded].seans) )

			Print #1, filename

			seans_str_in[seans_loaded].time_computer = date_2unixtime(_
			seans_str_in[seans_loaded].seans.Day1, _
			seans_str_in[seans_loaded].seans.Month1, _
			seans_str_in[seans_loaded].seans.Year1, _
			seans_str_in[seans_loaded].seans.Hour1, _
			seans_str_in[seans_loaded].seans.Minute1, _
			seans_str_in[seans_loaded].seans.Second1)

			seans_loaded += 1
			symbol = Print_process(symbol)
		End If
	Next i

	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i)
		If filename = "1" Then
			LoadFiles(directory + "/1")
			Exit For
		EndIf
	Next i

End Sub


'''==============================================


Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Long
	Return ( CPtr(seans_struct_in Ptr, elem1) -> time_computer ) - ( CPtr(seans_struct_in Ptr, elem2) -> time_computer )
End Function

