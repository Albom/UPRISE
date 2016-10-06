
#Include Once "albom_lib.bi"	   ' �������� ���������� "albom.dll"
#Include Once "albom_log.bi"		' ����������� ����
#Include Once "albom_as_file.bi"	' �������� �������� � �������� ��� ������ � AS-�������
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"

#Include "fbgfx.bi"			'����������� ����������� ����������


'''==============================================

#If __FB_LANG__ = "fb"
Using FB '��� �������� � ������������� ����� ��������
#EndIf


'''==============================================

Dim As Integer partrap(0 To 679)

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
Dim As Integer z ' ����� ������� ������

Dim As as_file_struct	as_file_in
Dim As as_file_struct 	as_file_out

Dim As Integer is_divide

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename
Dim As Integer file
Dim As Integer isTrapVar

Dim Shared As Double qLevel(0 To 255)
Dim Shared As Double qHint(0 To 255)
Dim Shared As Double qHkm(0 To 255)
Dim Shared As Double qTrap(0 To 255)
Dim Shared As Integer qNum = 0

Dim As Integer configPartrap

'''==============================================

file = FreeFile()
Open "config.dat" For Input As #file
Input #file, razr_filename
Input #file, isTrapVar
Close #file

If razr_load(razr_filename, @razr(0), 330) = 0 Then ' �������� ����������
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
Print "Approximate trapezoidal - ��������� �������� ��������� ������"
Print "(c) ������� �.�., ����� �.�. (�������� ���������)"
Print

Color 15

Input "������� ���� ������ ��������� (���� ����� ���): ", d_day, d_month, d_year
Input "������� ���������� �����: ", d_ndays

If isTrapVar = 0 Then
	Print
	Input "������� �������� ���������������� ������������: ", configPartrap
	Print
EndIf

'partrap = 0
'Input "������ �� ���������� ���������? (0 - ���, 1 - ��): ", is_divide
is_divide=1

If (is_divide <> 0) And (is_divide <> 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
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

		date_next(@d_day, @d_month, @d_year)

		If i = 0 Then
			DirectoryOutput += directory + "-"
		EndIf
	EndIf
Next i
DirectoryOutput += directory


Print
Print "������� ���������� ������� ������... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step1")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"

If isTrapVar <> 0 Then

	file = FreeFile()
	Open SEANS_DIR_OUT +DirectoryOutput+"/step1/config_trap.dat" For Input As #file

	While (Not Eof(file))
		Input #file, qLevel(qNum), qHint(qNum), qHkm(qNum), qTrap(qNum)
		qNum += 1
	Wend

	Close #file

	Print
	Print "������ ��������� ������/���:"
	For i = 0 To qNum-1
		Print Using "###.##     ####     ####.#     ###"; qLevel(i); qHint(i); qHkm(i); qTrap(i)
	Next i

	Color 12
	Print
	Print "������ ������� ���������? (Y/N) ";
	If GetKey_YN() = 0 Then
		Sleep 500
		End
	EndIf
	Color 15
	Print

EndIf


MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step2")


If isTrapVar = 0 Then
	file = FreeFile()
	Open SEANS_DIR_OUT +DirectoryOutput+"/step2/input.txt" For Output As #file
	Print #file, "�������� ���������������� ������������: "; configPartrap
	Close #file

	For h = 0 To 679
		partrap(h) = configPartrap
	Next h

Else

	file = FreeFile()
	Open SEANS_DIR_OUT +DirectoryOutput+"/step2/config_trap.dat" For Output As #file
	For i = 0 To qNum-1
		Print #file, Using "###.##     ####     ####.#     ###"; qLevel(i); qHint(i); qHkm(i); qTrap(i)
	Next i
	Close #file

	i = 0
	For h = 679 To 0 Step -1
		partrap(h) = qTrap(i)
		If h = qHint(i) Then
			i += 1
		EndIf
	Next h

EndIf

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput + "/step2/" + "profileTrap.txt" For Output As #file
For h = 0 To 679
	Print #file, Using "####.# ###"; seans2_altL(h); partrap(h)
Next h
Close #file




Print "������������ �� ������... ";

seans_num_out = seans_num_in



For t = 0 To seans_num_out-1 ' �� �������

	Print_process_percent((t*100)/seans_num_out)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step1/AS"+DirectoryOutput+"."+ext

	If as_file_load(filename, @as_file_in) = 0 Then ' ��������� ����� (��� ���� ���������� ������, ������� � ���������� ����� �����������)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
		End
	EndIf

	as_file_out.filename = as_file_in.filename
	as_file_out.date_    = as_file_in.date_
	as_file_out.time_    = as_file_in.time_
	as_file_out.nseans   = as_file_in.nseans
	as_file_out.tnak     = as_file_in.tnak

	For tau = 0 To 18  ' �� ��������
		as_file_out.rnc(tau) = as_file_in.rnc(tau)
		as_file_out.rns(tau) = as_file_in.rns(tau)
	Next tau

	as_file_out.nh = as_file_in.nh 'as_file_in.nh-partrap-18-partrap

	as_file_out.acf = Callocate(as_file_out.nh, SizeOf(acf_struct) )
	If as_file_out.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	Dim As Integer h_start = 0
	Dim As Integer h_end = 680

	For h = h_start To h_end-1

		as_file_out.acf[h].n = h
		as_file_out.acf[h].h = as_file_in.acf[h].h

	Next h



	' ��������� ����
	For h = h_start To h_end-1
		For tau = 0 To 18
			as_file_in.acf[h].rc(tau) -= as_file_in.rnc(tau)
			as_file_in.acf[h].rs(tau) -= as_file_in.rns(tau)
		Next tau
	Next h

	Dim As Double pNShort = 0
	For h = 500 To 599
		pNShort += as_file_in.acf[h].pShort
	Next h
	pNShort /= 100

	For h = h_start To h_end-1
		as_file_in.acf[h].pShort -= pNShort
	Next h




	' ���� ����������
	For h = h_start To h_end-1 ' �� ������
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
				as_file_in.acf[h].rc(tau) /= Sqr( r1*r2 )
				as_file_in.acf[h].rs(tau) /= Sqr( r1*r2 )
			Else
				as_file_in.acf[h].rc(tau) = 0
				as_file_in.acf[h].rs(tau) = 0
			EndIf

		Next tau

	Next h


	' ���� ���������� ��� ������� �������� �� ��������� ��������
	For h = h_start To h_end-1-12 ' �� ������

		Dim As Double r ' �������� ������������ ��������

		If h < 300-12 Then
			r = razr(h+12)
		Else
			r = 1.0
		EndIf

		If r > 1e-6 Then
			as_file_in.acf[h].pShort /= r
		Else
			as_file_in.acf[h].pShort = 0
		EndIf

	Next h


	For h = h_start To h_end-1 ' �� ������
		as_file_out.acf[h].pShort = as_file_in.acf[h].pShort
	Next h



	' 1) ��� ���������� ������������

	For h = 0 To as_file_in.nh-1

		If h >= 18+partrap(h) And h <= as_file_in.nh-partrap(h)-1 Then

			For tau = 0 To 18

				as_file_out.acf[h].rc(tau) = 0
				For z = h-tau-partrap(h) To h+partrap(h) ' �� ������
					as_file_out.acf[h].rc(tau) += as_file_in.acf[z].rc(tau)
				Next z

				If is_divide = 1 Then
					as_file_out.acf[h].rc(tau) /= tau+2*partrap(h)+1 ' ������ �� ���������� ���������
				EndIf

			Next tau

		Else

			For tau = 0 To 18
				as_file_out.acf[h].rc(tau) = 0
			Next tau

		EndIf

	Next h

	' 2) ��� �������� ������������

	For h = 0 To as_file_in.nh-1

		If h >= 18+partrap(h) And h <= as_file_in.nh-partrap(h)-1 Then

			For tau = 0 To 18

				as_file_out.acf[h].rs(tau) = 0
				For z = h-tau-partrap(h) To h+partrap(h) ' �� ������
					as_file_out.acf[h].rs(tau) += as_file_in.acf[z].rs(tau)
				Next z

				If is_divide = 1 Then
					as_file_out.acf[h].rs(tau) /= tau+2*partrap(h)+1 ' ������ �� ���������� ���������
				EndIf

			Next tau

		Else

			For tau = 0 To 18
				as_file_out.acf[h].rs(tau) = 0
			Next tau

		EndIf

	Next h

	' ������ ���������������� �������� � ��������� �/�, ������ � ������ ��������� �/�

	For h = 0 To as_file_in.nh-1

		as_file_out.acf[h].pcorr = as_file_in.acf[h].pcorr
		as_file_out.acf[h].qcorr = as_file_in.acf[h].qcorr

		as_file_out.acf[h].q = as_file_out.acf[h].rc(0)/as_file_out.rnc(0)


	Next h

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext



	as_file_save(filename, @as_file_out)



	DeAllocate(as_file_out.acf)   ' ����������� ������ � ���
	'	DeAllocate(as_file_out.param) ' ����������� ������ � �����������

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"


Print
Print "OK"
break

'''==============================================
