
#Include Once "albom_lib.bi"	'�������� ���������� "albom.dll"
#Include Once "albom_log.bi"		'����������� ����
#Include Once "albom_version.bi"
#Include "fbgfx.bi"			'����������� ����������� ����������

#Include  "crt/stdlib.bi"


#If __FB_LANG__ = "fb"
Using FB '��� �������� � ������������� ����� ��������
#EndIf

Type seans_struct
	Dim filename_full As ZString*256
	Dim filename As ZString*64
	Dim seans As seans2_data
	Dim isM As Integer
	Dim time_decimal As Double
	Dim time_computer As Integer
End Type

Dim filename As ZString*256 = ""
Dim filename_full As ZString*512 = ""
Dim directory As String
Dim As Integer i, j, k
Dim is1s As Integer
Dim As String SEANS_DIR = "./in/"
Dim As Integer symbol = 0 ' ��������������� ���������� ��� ����������� �������� �������� �������

Dim Shared timestr As ZString*16 ' ���������� ��� ������������� ������� � ���� ������

ReDim Preserve Shared As seans_struct seans_str(0 To 1)
Dim Shared seans_loaded As Integer   ' ������� ���������

Dim As Integer VISIBLE = 0 ' ����� �� ���������� �������?
Dim Shared As Integer CUR = 0 ' ������� ������� (������� �����)
Dim Shared As Integer H, HMIN, HMAX ' �������, ����������� � ������������ ������
Dim As Integer DX = 1 ' ������� �� ��� x
Dim As Integer DY = 200 ' ������� �� ��� y
Dim As Integer Y0 = 250 ' ��������� �������� �� ��� y
Dim As Integer START_X = 0
Dim As Integer POINTS_START = 0
Dim As Integer POINTS_END = 13
ReDim Shared vis_array(0 To 18, 0 To 10) As Double ' ����� ��� ����������� ��������
Dim Shared As Integer MAX_Y = 0
Dim Shared As Integer is_noise = 1 ' �������� ���? (1 - ���, -1 - ��)
Dim Shared As Integer CHANNEL = 1' ������� �����


Dim key As Integer

Declare Sub Vis_array_load()
Declare Sub AutomaticClear()
Declare Sub HelpPrint()
Declare Sub ACFPrint()
Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Integer

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

''' =======================================================================


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
Print "View - ��������� ��������� S-������ ������� �3"
Print "(c) ������� �.�., ����� �.�. (�������� ���������)"
Print

Color 15
Input "������� ���� ������ ��������� (���� ����� ���): ", d_day, d_month, d_year
Input "������� ���������� �����: ", d_ndays

Print "��������... ";

seans_loaded = 0

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

		LoadFiles(SEANS_DIR+directory)

		date_next(@d_day, @d_month, @d_year)

	EndIf

Next i


If seans_loaded = 0 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"

Print #1, Str(seans_loaded)+" files loaded"


' ���������� ������� �� �������
Print "���������� �� �������... ";
qsort(@seans_str(0), seans_loaded, SizeOf(seans_struct), @seans_struct_time_compare)
Print "OK"

HMIN = 44
HMAX = 665
H = HMIN

ReDim vis_array(0 To 18, 0 To seans_loaded) As Double ' ����� ��� ����������� ��������

Vis_array_load() ' ��������� ������ ��� �����������

Do

	ScreenLock '�������� ����� �� �����

	Cls

	If seans_str(CUR).seans.m(H) = 0 Then
		Line (5+(CUR-START_X)*DX, 25)-(5+(CUR-START_X)*DX, 768-25), 7
	Else
		Line (5+(CUR-START_X)*DX, 25)-(5+(CUR-START_X)*DX, 768-25), 7, , &b0000000011110000
	End If

	For j = POINTS_START To POINTS_END
		Line  (0, Y0+DY*j/6)-(1024, Y0+DY*j/6), 15-j, , &b0000000011110000
		For i = START_X To (seans_loaded-2)
			If seans_str(i).seans.m(H) <> 0 Then
				If VISIBLE = 1 Then Line(5+(i-START_X)*DX, Y0-DY*(vis_array(j, i))+DY*j/6)-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(j, i+1))+DY*j/6),  15, , &b0001000100010001 End If
			Else
				If seans_str(i+1).seans.m(H) <> 0 Then
					If VISIBLE = 1 Then Line(5+(i-START_X)*DX, Y0-DY*(vis_array(j, i))+DY*j/6)-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(j, i+1))+DY*j/6), 15, , &b0001000100010001 End If
				Else
					Line(5+(i-START_X)*DX, Y0-DY*(vis_array(j, i))+DY*j/6)-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(j, i+1))+DY*j/6),  15-j'+1
				End If
			End if
		Next i
	Next j


	' ��������� ����� ���������� �������
	For i = START_X To (seans_loaded-2)
		If seans_str(i).seans.m(H) <> 0 Then
			Line(5+(i-START_X)*DX, 768-24)-(5+(i+1-START_X)*DX, 768-21),  15, bf
		End If
	Next i

	' ����� ������������� ��������, �� ������� ����������� �������� ���
	Line(5+(MAX_Y-START_X)*DX, 768-24)-(5+(1+MAX_Y-START_X)*DX, 768-21),  12, bf



	Color 10
	Print seans_str(CUR).filename;

	If seans_str(CUR).isM = 2 Then
		Print  "*",
	Else
		Print " ",
	End If
	Color 14
	Print Using "�����: ###.###  "; seans_str(CUR).time_decimal;
	time_2str(seans_str(CUR).seans.Hour1, seans_str(CUR).seans.Minute1, seans_str(CUR).seans.Second1, @timestr)
	Print timestr,
	Color 11
	Print Using "������: � ###"; H+1;
	If H > HMAX-100 Then
		Print  "*";
	Else
		Print " ";
	End If
	Print Using "  #### ��"; seans2_altL(H);
	Color 10
	Print ,CHANNEL;
	Color 12
	Print ,,
	Print "F1: ������     Ctrl+Q: �����"
	Print


	ScreenUnLock '��������� ����� �� �����


	key = GetKey()

'		Cls
'		Print key
'		Sleep 1000

	Select Case key

		Case KEY_CTRL_P
			BSave ("screen.bmp", 0)

		Case KEY_RIGHT
			If CUR < seans_loaded-1 Then CUR = CUR + 1 End If

		Case KEY_LEFT
			If CUR > 0 Then CUR = CUR - 1  End If

		Case KEY_PLUS
			If DX < 32 Then DX = DX*2 End If

		Case KEY_MINUS
			If DX > 1 Then DX = DX/2 End If

		Case KEY_CTRL_LEFT
			If START_X < seans_loaded-1 Then START_X = START_X + 1 End If

		Case KEY_CTRL_RIGHT
			If START_X > 0  Then START_X = START_X - 1  End If

		Case KEY_H
			Color 15
			Input "H: ", H
			H = H-1
			If H < HMIN Then H = HMIN End If
			If H > HMAX-1 Then H = HMAX-1 End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_O
			Color 15
			Input "O ( 0 ... 13): ", POINTS_START
			If POINTS_START < 0 Then POINTS_START = 0 End If
			If POINTS_START > 13 Then POINTS_START = 13 End If
			Print Using "O (## ... 13)"; POINTS_START;
			Input ": ", POINTS_END
			If POINTS_END < POINTS_START Then POINTS_END = POINTS_START End If
			If POINTS_END > 13 Then POINTS_END = 13 End If

		Case KEY_PAGE_UP
			If H < HMAX-1 Then H = H + 1 End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_PAGE_DOWN
			If H > HMIN Then H = H - 1 End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_SPACE
			If seans_str(CUR).seans.m(H) = 0 Then
				seans_str(CUR).seans.m(H) = 1
			Else
				seans_str(CUR).seans.m(H) = 0
			End If
			seans_str(CUR).isM = 2
			seans2_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_V
			If VISIBLE = 0 Then
				VISIBLE = 1
			Else
				VISIBLE = 0
			End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_F1
			HelpPrint()
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_R
			ACFPrint()
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_HOME
			CUR=START_X

		Case KEY_END
			If START_X+(1024\DX)-1 < seans_loaded Then
				CUR = START_X+(1024\DX)-1
			Else
				CUR=seans_loaded-1
			End If

		Case KEY_D
			Dim As Integer fileTemp
			Dim As Double pN1, pN2

			pN1 = 0
			pN2 = 0
			For i = 650 To 670
				pN1 += seans_str(CUR).seans.datps1(i)
				pN2 += seans_str(CUR).seans.datps2(i)
			Next i
			pN1 /= 20
			pN2 /= 20

			fileTemp = FreeFile()
			Open "S1-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + "__" + Str(seans_str(CUR).seans.Hour1) + "."+Str(seans_str(CUR).seans.Minute1) + "."+Str(seans_str(CUR).seans.Second1) + ".csv" For Output As #fileTemp
			For i = 0 To 679
				Print #fileTemp, Using "####.#; ####.####"; seans2_altS(i); (seans_str(CUR).seans.datps1(i)-pN1)/pN1
			Next i
			Close #fileTemp

			fileTemp = FreeFile()
			Open "S2-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + "__" + Str(seans_str(CUR).seans.Hour1) + "."+Str(seans_str(CUR).seans.Minute1) + "."+Str(seans_str(CUR).seans.Second1) + ".csv" For Output As #fileTemp
			For i = 0 To 679
				Print #fileTemp, Using "####.#; ####.####"; seans2_altS(i); (seans_str(CUR).seans.datps2(i)-pN2)/pN2
			Next i
			Close #fileTemp

			pN1 = 0
			pN2 = 0
			For i = 650 To 670
				pN1 += seans_str(CUR).seans.dat1(i, 0)
				pN2 += seans_str(CUR).seans.dat3(i, 0)
			Next i
			pN1 /= 20
			pN2 /= 20


			fileTemp = FreeFile()
			Open "L1-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + "__" + Str(seans_str(CUR).seans.Hour1) + "."+Str(seans_str(CUR).seans.Minute1)  + "."+Str(seans_str(CUR).seans.Second1) + ".csv" For Output As #fileTemp
			For i = 0 To 679
				Print #fileTemp, Using "####.#; ####.####"; seans2_altL(i); (seans_str(CUR).seans.dat1(i, 0)-pN1)/pN1
			Next i
			Close #fileTemp

			fileTemp = FreeFile()
			Open "L2-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + "__" + Str(seans_str(CUR).seans.Hour1) + "."+Str(seans_str(CUR).seans.Minute1) +  "."+Str(seans_str(CUR).seans.Second1) + ".csv" For Output As #fileTemp
			For i = 0 To 679
				Print #fileTemp, Using "####.#; ####.####"; seans2_altL(i); (seans_str(CUR).seans.dat3(i, 0)-pN2)/pN2
			Next i
			Close #fileTemp


		Case KEY_Z

			ReDim As Double zt1(0 To seans_loaded-1)
			ReDim As Double zt2(0 To seans_loaded-1)
			ReDim As Double zt3(0 To seans_loaded-1)
			ReDim As Double zt4(0 To seans_loaded-1)

			ReDim As Double ztr1(0 To seans_loaded-1)
			ReDim As Double ztr2(0 To seans_loaded-1)

			ReDim As Double rnz1(0 To 18, 0 To seans_loaded-1)
			ReDim As Double rnz2(0 To 18, 0 To seans_loaded-1)

			ReDim As Double rnz1s(0 To 18, 0 To seans_loaded-1)
			ReDim As Double rnz2s(0 To 18, 0 To seans_loaded-1)

			Dim As Integer t

			Dim As Integer file1, file2, file3, file4
			Dim As Integer file5, file6, file7, file8
			Dim As Integer file9, file10

			file1 = FreeFile()
			Open "Z1-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file1

			file2 = FreeFile()
			Open "Z2-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file2

			file3 = FreeFile()
			Open "Z3-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file3

			file4 = FreeFile()
			Open "Z4-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file4

			file5 = FreeFile()
			Open "Z1r-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file5

			file6 = FreeFile()
			Open "Z2r-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file6

			file7 = FreeFile()
			Open "RNZ1-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file7

			file8 = FreeFile()
			Open "RNZ2-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file8

			file9 = FreeFile()
			Open "RNZ1s-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file9

			file10 = FreeFile()
			Open "RNZ2s-"+ Str(seans_str(CUR).seans.Year1) + "-" + Str(seans_str(CUR).seans.Month1) + "-"+ Str(seans_str(CUR).seans.Day1) + ".csv" For Output As #file10


			For t = 0 To seans_loaded-1
				zt1(t) = 0
				zt2(t) = 0
				zt3(t) = 0
				zt4(t) = 0

				ztr1(t) = 0
				ztr2(t) = 0

				For tau As Integer = 0 To 18
					rnz1(tau, t) = 0
					rnz2(tau, t) = 0
					rnz1s(tau, t) = 0
					rnz2s(tau, t) = 0
				Next tau

				Dim As Integer numH = 0

				For i = 500 To 600
					zt1(t) += seans_str(t).seans.dat01(i)
					zt2(t) += seans_str(t).seans.dat02(i)
					zt3(t) += seans_str(t).seans.dat03(i)
					zt4(t) += seans_str(t).seans.dat04(i)

					For tau As Integer = 0 To 18
						rnz1(tau, t) += seans_str(t).seans.dat1(i, tau)
						rnz2(tau, t) += seans_str(t).seans.dat3(i, tau)
						rnz1s(tau, t) += seans_str(t).seans.dat2(i, tau)
						rnz2s(tau, t) += seans_str(t).seans.dat4(i, tau)
					Next tau

					For tau As Integer = 15 To 18
						ztr1(t) += seans_str(t).seans.dat1(i, tau)
						ztr2(t) += seans_str(t).seans.dat3(i, tau)
					Next tau

					numH += 1
				Next i

				zt1(t) /= numH
				zt2(t) /= numH
				zt3(t) /= numH
				zt4(t) /= numH

				ztr1(t) /= 4*numH
				ztr2(t) /= 4*numH

				For tau As Integer = 0 To 18
					rnz1(tau, t) /= numH
					rnz2(tau, t) /= numH
					rnz1s(tau, t) /= numH
					rnz2s(tau, t) /= numH
				Next tau

				For tau As Integer = 0 To 18
					rnz1(tau, t) -= zt1(t)*zt1(t)/1463.0
					rnz2(tau, t) -= zt2(t)*zt2(t)/1463.0
				Next tau

				Print #file1, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; zt1(t)
				Print #file2, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; zt2(t)
				Print #file3, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; zt3(t)
				Print #file4, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; zt4(t)

				Print #file5, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; ztr1(t)
				Print #file6, Using "##.####; ##.####^^^"; seans_str(t).time_decimal; ztr2(t)

				For tau As Integer = 0 To 18
					Print #file7, Using "######.####; "; rnz1(tau, t);'/rnz1(0, t);
					Print #file8, Using "######.####; "; rnz2(tau, t);'/rnz2(0, t);
					Print #file9,  Using "######.####; "; rnz1s(tau, t);
					Print #file10, Using "######.####; "; rnz2s(tau, t);
				Next tau
				Print #file7,
				Print #file8,
				Print #file9,
				Print #file10,

				/'
				For i As Integer = 0 To 679
					Print #file7, Using "######.####; "; seans_str(t).seans.dat01(i);
					Print #file8, Using "######.####; "; seans_str(t).seans.dat02(i);
				Next i
				Print #file7,
				Print #file8,
'/
			Next t

			Close #file1
			Close #file2
			Close #file3
			Close #file4
			Close #file5
			Close #file6
			Close #file7
			Close #file8
			Close #file9
			Close #file10

/'
		Case KEY_I
			ReDim As Double p(0 To 679, 0 To seans_loaded-1)
			Dim As Integer file


			For h As Integer = 0 To 679
				For t As Integer = 0 To seans_loaded-1
					p(h, t) = 0
					For tau As Integer = 0 To 18
						p(h, t) += seans_str(t).seans.dat1(h, tau)
					Next tau
				Next t
			Next h

			file = FreeFile()
			Open "p.csv" For Output As #file

			For h As Integer = 0 To 679 Step 1
				For t As Integer = 0 To seans_loaded-1 Step 1
					Print #file, Using "##.###^^^; "; p(h, t);
				Next t
				Print #file,
			Next h

			Close #file

			file = FreeFile()
			Open "p2.csv" For Output As #file
			For h As Integer = 0 To 679 Step 1
				For t As Integer = 0 To seans_loaded-1 Step 1
					Print #file, Using "##.###^^^^; "; seans_str(t).seans.dat1(h, 0);
				Next t
				Print #file,
			Next h
			Close #file


			file = FreeFile()
			Open "t.csv" For Output As #file
			For t As Integer = 0 To seans_loaded-1 Step 1
				Print #file, Using "##.###^^^^; "; seans_str(t).time_decimal;
			Next t
			Close #file


			file = FreeFile()
			Open "h.csv" For Output As #file
			For h As Integer = 0 To 679 Step 1
				Print #file, Using "##.###^^^^; "; seans2_altL_front(h)
			Next h
			Close #file
'/

		Case KEY_A
			Color 15
			AutomaticClear()
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_TAB
			If CUR + 10 < seans_loaded-1 Then CUR = CUR + 10

		Case KEY_N
			is_noise = -is_noise
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_DEL
			seans_str(CUR).seans.m(H) = 1
			seans_str(CUR).isM = 2
			seans2_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			If CUR < seans_loaded-1 Then CUR = CUR + 1 End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_CTRL_DEL
			For i = 0 To 679
				seans_str(CUR).seans.m(i) = 1
			Next i
			seans_str(CUR).isM = 2
			seans2_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			Vis_array_load ' ��������� ������ ��� �����������			

		Case KEY_BACKSPACE
			seans_str(CUR).seans.m(H) = 1
			seans_str(CUR).isM = 2
			seans2_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			If CUR > 0 Then CUR = CUR - 1  End If
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_1
			CHANNEL = 1
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_2
			CHANNEL = 2
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_3
			CHANNEL = 3
			Vis_array_load ' ��������� ������ ��� �����������

		Case KEY_4
			CHANNEL = 4
			Vis_array_load ' ��������� ������ ��� �����������





	End Select


	If key = KEY_CTRL_Q Then Exit Do End If

Loop


' DeAllocate (seans_str)


''' =======================================================================




Sub HelpPrint

	Dim key As Integer

	Cls
	Color 11
	Print "UPRISE version " + UPRISE_VERSION
	Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
	Print
	Color 7
	Print "View - ��������� ��������� S-������ ������� �3 (��������������� �����������)"
	Print "(c) ������� �.�., ����� �.�. (�������� ���������)"
	Color 8
	Print
	Print "================================"
	Print "��������� ������� " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
	Print "================================"
	Color 15
	Print
	Print "����������:"
	Print " Alt + Enter      ������������� �� �������� ������ � ������������� � �������� (�������� �� �� ���� ��������)"
	Print "       Left Right ����������� �� �������"
	Print "       Home       ����������� ������� �� ����� � ������ ������"
	Print "       End        ����������� ������� �� ����� � ����� ������"
	Print "       Tab        ����������� ������� �� 10 ������� ������"
	Print "Ctrl + Left Right ����� ������� �� ��� �������"
	Print "        +   -     ��������� �������� �� ��� �������"
	Print "       PgUp PgDn  ����������� �� �������"
	Print "       H          ������ ������� ������"
	Print "       ������     �������� ������ �� ������� ������ �� �������� ������"
	Print "       Del        �������� ������ �� ������� ������ �� �������� ������ � �������� ������ �� ��������� �����"
	Print "       BackSpace  �������� ������ �� ������� ������ �� �������� ������ � �������� ������ �� ���������� �����"
	Print "       V          ���������� ��� �������� ���������� ������"
	Print "       O          ����� ������� ��� �����������"
	Print "       R          �������� ����������� ���������� ������� ��"
	Print "       N          ����� ������ - ������+��� ��� ������ (����������� ����������)"
	Print "       1 2 3 4    ����� ������"
	Print "       A          �������������� �������� ����������� ���������"
	Print "       F1         ����� ���� ������"
	Print "Ctrl + P          ���������� ������ � ���� screen.bmp"
	Print "Ctrl + Q          ����� �� ���������"
	Print
	Color 12
	Print "������� Enter"

	Do
		key = GetKey
	Loop Until key = KEY_ENTER

	Color 15, 0
	Cls
End Sub


''' =======================================================================



Sub Vis_array_load()

	Dim As Integer i, j
	Dim As Double d
	Dim As Integer NUM = 15

	ReDim As Double max_a(0 To seans_loaded-1) ' ������ ��� ������ max (� ������ �����)
	ReDim As Double noise(0 To seans_loaded-1, 0 To 18) ' ������ ��� ����

	If is_noise < 0 Then
		Select Case CHANNEL
			Case 1
				For i = 0 To seans_loaded-1
					seans2_noise1(@seans_str(i).seans, @noise(i, 0), 19, 500, 600)
				Next i
			Case 2
				For i = 0 To seans_loaded-1
					seans2_noise2(@seans_str(i).seans, @noise(i, 0), 19, 500, 600)
				Next i
			Case 3
				For i = 0 To seans_loaded-1
					seans2_noise3(@seans_str(i).seans, @noise(i, 0), 19, 500, 600)
				Next i
			Case 4
				For i = 0 To seans_loaded-1
					seans2_noise4(@seans_str(i).seans, @noise(i, 0), 19, 500, 600)
				Next i
			Case 2
		End Select
	Else
		For i = 0 To seans_loaded-1
			For j = 0 To 18
				noise(i, j) = 0
			Next
		Next
	EndIf


	For j = 0 To NUM ' ��������� �������� ��� �����������
		For i = 0 To seans_loaded-1
			Select Case CHANNEL
				Case 1
					vis_array(j, i) = seans_str(i).seans.dat1(H, j) - noise(i, j)
				Case 2
					vis_array(j, i) = seans_str(i).seans.dat2(H, j) - noise(i, j)
				Case 3
					vis_array(j, i) = seans_str(i).seans.dat3(H, j) - noise(i, j)
				Case 4
					vis_array(j, i) = seans_str(i).seans.dat4(H, j) - noise(i, j)
			End Select
		Next i
	Next j


	If is_noise < 0 Then
		For i = 0 To seans_loaded-1
			For j = 18 To 0 Step -1
				vis_array(j, i) /= vis_array(0, i)
			Next
		Next
	EndIf

	d = -1e200
	For i = 0 To seans_loaded-1
		If vis_array(0, i) > d And seans_str(i).seans.m(H) = 0 Then
			d = vis_array(0, i) : MAX_Y = i
		EndIf
	Next i

	For j = 0 To NUM
		For i = 0 To seans_loaded-1
			vis_array(j, i) = vis_array(j, i) / d
		Next i
	Next j

End Sub


''' =======================================================================


Sub AutomaticClear()

	Dim As Integer wnd_width
	Dim As Double lev
	Dim As Integer h
	Dim As Integer ord, t, sm
	Dim As Double wnd(0 To 100)
	Dim As Double mean
	Dim As Integer num
	Dim As Double dev
	Dim As Integer mode
	Dim As Integer symbol

	Print "�������������� ���������� ����������� ���������"
	Print
	Print "����������� ���������: ������ ���� - 15, ������� - 4.5"
	Print
	Print "0: �����"
	Print "1: ������� ��� �����"
	Print "2: �������� ���������� �����"
	Print "3: �������� ��������� �����"
	Input "[0..3]: ", mode
	Print

	Select Case mode

		Case 0
			Exit Sub

		Case 1
			For t = 0 To seans_loaded-1 ' �� �������
				seans_str(t).isM = 1
				seans2_saveM0(seans_str(t).filename_full)
				symbol = Print_process(symbol)
				For i As Integer = 0 To 679
					seans_str(t).seans.m(i) = 0
				Next i
			Next t

		Case 2 ' �������� ���������� �����
			Input "������ ����: ", wnd_width
			Input "�������: ", lev

			For ord = 0 To 12 ' �� ���������
				For h As Integer = 0 To 679 ' �� ������
					For t = seans_loaded-1-wnd_width To 1 Step -1 ' �� �������

						num = 0
						mean = 0
						For sm = 0 To wnd_width-1
							If seans_str(t+sm).seans.m(h) = 0 Then
								mean += seans_str(t+sm).seans.dat1(h, ord)
								num += 1
							EndIf
						Next sm

						If num > 9 Then
							mean /= num

							dev = 0
							For sm = 0 To wnd_width-1
								If seans_str(t+sm).seans.m(h) = 0 Then
									dev += ( seans_str(t+sm).seans.dat1(h, ord) - mean )^2
								EndIf
							Next sm

							dev = Sqr(dev/(num-1))

							If Abs(seans_str(t-1).seans.dat1(h, ord)-mean) > lev*dev Then
								seans_str(t-1).seans.m(h) = 1
							EndIf

						EndIf

					Next t
				Next h
			Next ord

			For t = 0 To seans_loaded-1
				seans_str(t).isM = 2
				seans2_saveM3(seans_str(t).filename_full, @(seans_str(t).seans))
				symbol = Print_process(symbol)
			Next t

		Case 3 ' �������� ��������� �����
			Input "������ ����: ", wnd_width
			Input "�������: ", lev

			For ord = 0 To 12 ' �� ���������
				For h As Integer = 0 To 679 ' �� ������
					For t = 0 To seans_loaded-1-wnd_width ' �� �������

						num = 0
						mean = 0
						For sm = 0 To wnd_width-1
							If seans_str(t+sm).seans.m(h) = 0 Then
								mean += seans_str(t+sm).seans.dat1(h, ord)
								num += 1
							EndIf
						Next sm

						If num > 9 Then
							mean /= num

							dev = 0
							For sm = 0 To wnd_width-1
								If seans_str(t+sm).seans.m(h) = 0 Then
									dev += ( seans_str(t+sm).seans.dat1(h, ord) - mean )^2
								EndIf
							Next sm

							dev = Sqr(dev/(num-1))

							If Abs(seans_str(t+wnd_width).seans.dat1(h, ord)-mean) > lev*dev Then
								seans_str(t+wnd_width).seans.m(h) = 1
							EndIf

						EndIf

					Next t
				Next h
			Next ord

			For t = 0 To seans_loaded-1
				seans_str(t).isM = 2
				seans2_saveM3(seans_str(t).filename_full, @(seans_str(t).seans))
				symbol = Print_process(symbol)
			Next t


	End Select

End Sub



''' =======================================================================



Sub ACFPrint()

	Dim key As Integer
	Dim As Integer i, j, k
	Dim As Integer tau
	Dim As Integer x0, dxdy
	Dim As Double r(0 To 18)'��� ������� ��
	Dim As Double n(0 To 18)'��� ����
	Dim As Double sn
	Dim As Integer is_noise_ACF = 1
	Dim As Integer is_R0_ACF = 1

	Dim As Double R0DAC_1(0 To 18) ' 0 ��� 1-�� ����������� ������
	Dim As Double R0DAC_2(0 To 18) ' 0 ��� 1-�� ��������� ������
	Dim As Double R0DAC_3(0 To 18) ' 0 ��� 2-�� ����������� ������
	Dim As Double R0DAC_4(0 To 18) ' 0 ��� 2-�� ��������� ������

	Dim As Integer curVariancePoint = 0 ' ������� ����� ��������� (�� 0 �� 18)

	Do

		For i = 0 To 18
			n(i) = 0
		Next i

		k = 0
		For j = HMAX-100 To HMAX-10
			If seans_str(CUR).seans.m(j) = 0 Then
				If is_R0_ACF > 0 Then ' ��������� ���� ���?

					If h < 679-19 Then
						For tau = 0 To 18
							R0DAC_1(tau) = CDbl(seans_str(CUR).seans.dat01(j))*CDbl(seans_str(CUR).seans.dat01(j+tau))/1463.0
							R0DAC_3(tau) = CDbl(seans_str(CUR).seans.dat02(j))*CDbl(seans_str(CUR).seans.dat02(j+tau))/1463.0
							R0DAC_2(tau) = CDbl(seans_str(CUR).seans.dat01(j))*CDbl(seans_str(CUR).seans.dat02(j+tau))/1463.0
							R0DAC_4(tau) = CDbl(seans_str(CUR).seans.dat02(j))*CDbl(seans_str(CUR).seans.dat01(j+tau))/1463.0
						Next tau
					Else
						For tau = 0 To 18
							R0DAC_1(tau) = 0
							R0DAC_3(tau) = 0
							R0DAC_2(tau) = 0
							R0DAC_4(tau) = 0
						Next tau
					EndIf

					For i = 0 To 18
						Select Case CHANNEL
							Case 1
								n(i) += seans_str(CUR).seans.dat1(j, i) - R0DAC_1(i)
							Case 2
								n(i) += seans_str(CUR).seans.dat2(j, i) - R0DAC_2(i)
							Case 3
								n(i) += seans_str(CUR).seans.dat3(j, i) - R0DAC_3(i)
							Case 4
								n(i) += seans_str(CUR).seans.dat4(j, i) - R0DAC_4(i)
						End Select
					Next i
				Else
					For i = 0 To 18
						Select Case CHANNEL
							Case 1
								n(i) += seans_str(CUR).seans.dat1(j, i)
							Case 2
								n(i) += seans_str(CUR).seans.dat2(j, i)
							Case 3
								n(i) += seans_str(CUR).seans.dat3(j, i)
							Case 4
								n(i) += seans_str(CUR).seans.dat4(j, i)
						End Select
					Next i
				EndIf
				k += 1
			EndIf
		Next j

		array_norm_d(@n(0), @n(0), k, 19) ' ����������� �� ���������� ������� ����� k � ������� �������

		If is_noise_ACF > 0 Then ' �������� ��� ��� ���?
			For i = 0 To 18
				Select Case CHANNEL
					Case 1
						r(i) = seans_str(CUR).seans.dat1(H, i) - n(i)
					Case 2
						r(i) = seans_str(CUR).seans.dat2(H, i) - n(i)
					Case 3
						r(i) = seans_str(CUR).seans.dat3(H, i) - n(i)
					Case 4
						r(i) = seans_str(CUR).seans.dat4(H, i) - n(i)
				End Select
			Next i
		Else
			For i = 0 To 18
				Select Case CHANNEL
					Case 1
						r(i) = seans_str(CUR).seans.dat1(H, i)
					Case 2
						r(i) = seans_str(CUR).seans.dat2(H, i)
					Case 3
						r(i) = seans_str(CUR).seans.dat3(H, i)
					Case 4
						r(i) = seans_str(CUR).seans.dat4(H, i)
				End Select
			Next i
		EndIf


		If h < 679-19 Then
			For tau = 0 To 18
				R0DAC_1(tau) = CDbl(seans_str(CUR).seans.dat01(h))*CDbl(seans_str(CUR).seans.dat01(h+tau))/1463.0
				R0DAC_3(tau) = CDbl(seans_str(CUR).seans.dat02(h))*CDbl(seans_str(CUR).seans.dat02(h+tau))/1463.0
				R0DAC_2(tau) = CDbl(seans_str(CUR).seans.dat01(h))*CDbl(seans_str(CUR).seans.dat02(h+tau))/1463.0
				R0DAC_4(tau) = CDbl(seans_str(CUR).seans.dat02(h))*CDbl(seans_str(CUR).seans.dat01(h+tau))/1463.0
			Next tau
		Else
			For tau = 0 To 18
				R0DAC_1(tau) = 0
				R0DAC_3(tau) = 0
				R0DAC_2(tau) = 0
				R0DAC_4(tau) = 0
			Next tau
		EndIf

		If is_R0_ACF > 0 Then ' ��������� ���� ���?
			For i = 0 To 18
				Select Case CHANNEL
					Case 1
						r(i) -= R0DAC_1(i)
					Case 2
						r(i) -= R0DAC_2(i)
					Case 3
						r(i) -= R0DAC_3(i)
					Case 4
						r(i) -= R0DAC_4(i)
				End Select
			Next i
		EndIf

		If n(0) <> 0 Then
			sn = r(0)/n(0)
		Else
			sn = 0
		EndIf

		array_norm0_d(@r(0), @r(0), 19)
		array_norm0_d(@n(0), @n(0), 19)


		' ������ ��������� ������ ����� ���

		ReDim As Double r15(0 To 18, -7 To 7)
		ReDim As Double n15(0 To 18, -7 To 7)
		ReDim As Double m15(0 To 18) ' ���. �������� (mean)
		ReDim As Double v15(0 To 18) ' ��������� (variance)
		ReDim As Double vn15(0 To 18) ' ������������� ���������

		If CUR > 7 And CUR < seans_loaded-7 Then

			For t As Integer = -7 To 7

				For tau = 0 To 18
					n15(tau, t) = 0
					k = 0
					For z As Integer = HMAX-100 To HMAX-10
						If seans_str(CUR+t).seans.m(z) = 0 Then
							n15(tau, t) += seans_str(CUR+t).seans.dat1(z, tau)
							k += 1
						EndIf
					Next z
					If k <> 0 Then
						n15(tau, t) /= k
					Else
						n15(tau, t) = 0
					EndIf

					r15(tau, t) = seans_str(CUR+t).seans.dat1(H, tau) - n15(tau, t)

				Next tau

			Next t

			For tau = 0 To 18
				m15(tau) = 0
				k = 0
				For t As Integer = -7 To 7
					If seans_str(CUR+t).seans.m(H) = 0 Then
						m15(tau) += r15(tau, t)
						k += 1
					EndIf
				Next t
				If k > 9 Then
					m15(tau) /= k
				Else
					m15(tau) = 0
				EndIf

			Next tau

			For tau = 0 To 18
				v15(tau) = 0
				k = 0
				For t As Integer = -7 To 7
					If seans_str(CUR+t).seans.m(H) = 0 Then
						v15(tau) += ( r15(tau, t)-m15(tau) )^2
						k += 1
					EndIf
				Next t
				If k > 9 Then
					v15(tau) /= k
				Else
					v15(tau) = 0
				EndIf
			Next tau

			For tau = 0 To 18
				v15(tau) /= 1-30.555*tau/663.0
			Next


			For tau = 0 To 18
				vn15(tau) = v15(tau)/v15(0)
			Next

		EndIf



		' ���������

		Cls

		Color 11
		If is_noise_ACF > 0 Then  ' �������� ��� ��� ���?
			Print "�����: �  ";
			Color 13
			Print Using "�/� = ###.##   "; sn;
		Else
			Print "�����: �+�               ";
		EndIf

		Color 15
		Print Using "�����: ###.###    "; seans_str(CUR).time_decimal;
		time_2str(seans_str(CUR).seans.Hour1, seans_str(CUR).seans.Minute1, seans_str(CUR).seans.Second1, @timestr)
		Print timestr,

		Color 11
		Print Using "������: � ###"; H+1;
		Print Using "  #### ��  "; seans2_altL(H);

		Color 14
		If is_R0_ACF > 0 Then
			Print "-0";
		Else
			Print "+0";
		EndIf

		Color 10
		Print "   ��������� �"; curVariancePoint; " �����:"; CInt(v15(curVariancePoint)/10000)


		Color 7
		Print
		Print "����������� �� �������: �����/������     ����������� �� ����� ���: �����/����     ����������� �� ������: PageDown/PageUp"
		Print "� / �+�: N                               '0' ���: A"
		Color 12
		Print
		Print "��� ������ ������� Enter"

		x0 = 50
		dxdy = 50

		Line (x0, 400-dxdy*5)-(x0+18*dxdy, 400-dxdy*5), 7, , &b0000000011110000
		Line (x0, 400-dxdy*4)-(x0+18*dxdy, 400-dxdy*4), 7, , &b0000000011110000
		Line (x0, 400-dxdy*3)-(x0+18*dxdy, 400-dxdy*3), 7, , &b0000000011110000
		Line (x0, 400-dxdy*2)-(x0+18*dxdy, 400-dxdy*2), 7, , &b0000000011110000
		Line (x0, 400-dxdy*1)-(x0+18*dxdy, 400-dxdy*1), 7, , &b0000000011110000
		Line (x0, 400)-(x0+18*dxdy, 400), 7
		Line (x0, 400+dxdy*1)-(x0+18*dxdy, 400+dxdy*1), 7, , &b0000000011110000
		Line (x0, 400+dxdy*2)-(x0+18*dxdy, 400+dxdy*2), 7, , &b0000000011110000

		For i = 0 To 18
			Line(x0+i*dxdy, 400-dxdy*5)-(x0+i*dxdy, 400+dxdy*2), 7, , &b0000000011110000
		Next i

		For i = 0 To 17
			Line (x0+i*dxdy, 400-5*dxdy*r(i))-(x0+(i+1)*dxdy, 400-5*dxdy*r(i+1)), 10
			Line (x0+i*dxdy, 400-5*dxdy*n(i))-(x0+(i+1)*dxdy, 400-5*dxdy*n(i+1)), 9
		Next i

		For i = 0 To 18
			Circle (x0+i*dxdy, 400-5*dxdy*r(i)), 3, 10
			Circle (x0+i*dxdy, 400-5*dxdy*n(i)), 3, 9
		Next i

		For i = 1 To 9
			Draw String (x0+i*dxdy*2+2-12, 400+2*dxdy+2), Str(CInt(i*30.555*2)) /'Left(Str(i*30.555*2), 5)'/, 15
		Next i

		For i = 0 To 4 ' ������� ������� �� 1 �� 0.2
			Draw String (x0-32, 400-5*dxdy+i*dxdy), Left(Str( (100-20*i)/100 ), 5), 15
		Next i

		' ������� ������� 0
		Draw String (x0-40, 400-5*dxdy+i*dxdy), "   0", 15

		For i = 6 To 7 ' ������� ������� �� -0.2 �� -0.4
			Draw String (x0-40, 400-5*dxdy+i*dxdy), Left(Str( (100-20*i)/100 ), 5), 15
		Next i

		' ����� ���������

		Line (x0, 700-dxdy*3)-(x0+18*dxdy, 700-dxdy*3), 7, , &b0000000011110000
		Line (x0, 700-dxdy*2)-(x0+18*dxdy, 700-dxdy*2), 7, , &b0000000011110000
		Line (x0, 700-dxdy*1)-(x0+18*dxdy, 700-dxdy*1), 7, , &b0000000011110000
		Line (x0, 700)-(x0+18*dxdy, 700), 7
		Line (x0, 700+dxdy*1)-(x0+18*dxdy, 700+dxdy*1), 7, , &b0000000011110000

		For i = 0 To 18
			Line(x0+i*dxdy, 700-dxdy*3)-(x0+i*dxdy, 700+dxdy*1), 7, , &b0000000011110000
		Next i

		For i = 0 To 4 ' ������� ������� �� 3 �� -2
			Draw String (x0-32, 700-2*dxdy+i*dxdy), Left(Str( 3-i ), 5), 15
		Next i

		If CUR > 7 And CUR < seans_loaded-7 Then

			For i = 0 To 17
				Line (x0+i*dxdy, 700+dxdy-dxdy*vn15(i))-(x0+(i+1)*dxdy, 700+dxdy-dxdy*vn15(i+1)), 12
			Next i

			For i = 0 To 18
				Circle (x0+i*dxdy, 700+dxdy-dxdy*vn15(i)), 3, 12
			Next i

		EndIf


		key = GetKey

		Select Case key

			Case KEY_UP
				If curVariancePoint < 18 Then
					curVariancePoint += 1
				EndIf

			Case KEY_DOWN
				If curVariancePoint > 0 Then
					curVariancePoint -= 1
				EndIf

			Case KEY_RIGHT
				If CUR < seans_loaded-1 Then CUR = CUR + 1 End If

			Case KEY_LEFT
				If CUR > 0 Then CUR = CUR - 1  End If

			Case KEY_N
				is_noise_ACF *= -1

			Case KEY_A
				is_R0_ACF *= -1

			Case KEY_PAGE_UP
				If H < HMAX-1 Then H += 1 End If
				Vis_array_load() ' ��������� ������ ��� �����������

			Case KEY_PAGE_DOWN
				If H > HMIN Then H -= 1 End If
				Vis_array_load() ' ��������� ������ ��� �����������

		End Select


	Loop Until key = KEY_ENTER

	Color 15, 0
	Cls
End Sub



''' =======================================================================



Sub LoadFiles(ByVal Directory As String)

	Dim As ZString Ptr lst  ' ������ ������
	Dim As Integer lst_len  ' ����� ������ ������
	Dim As Integer seans_num' ���������� ������� � ����������

	Dim As ZString*256 filename
	Dim As Integer isM

	Dim As Integer i
	Dim As Integer symbol = 0

	lst = Allocate (2*1024*1024*SizeOf(Byte)) ' �������� �� ������ ������ 2 ������
	If lst = NULL Then
		Print "������ ������!!!", seans_loaded+seans_num, Directory
		break
	EndIf


	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) '�������� ��� ����� �� ������
		If seans2_test(Directory+"/"+filename) > 0 Then
			seans_num += 1
		EndIf
	Next i

	Print seans_loaded+seans_num,

	ReDim Preserve As seans_struct seans_str(0 To seans_loaded+seans_num-1)

	/'
	If Err = 4 Then
		Print "������ ������!!!", seans_loaded+seans_num, Directory
		break
	EndIf
'/

	For i = 0 To lst_len-1

		filelist_get_filename(lst, @filename, i)

		isM = seans2_test(directory + "/" + filename)

		If (isM <> 0) Then
			seans2_load ( directory + "/" + filename, @(seans_str(seans_loaded).seans) )

			Print #1, filename

			seans_str(seans_loaded).filename = filename
			seans_str(seans_loaded).filename_full = directory + "/" + filename
			seans_str(seans_loaded).isM = isM

			seans_str(seans_loaded).time_decimal = time_2decimal(_
			seans_str(seans_loaded).seans.Hour1, _
			seans_str(seans_loaded).seans.Minute1, _
			seans_str(seans_loaded).seans.Second1)
			seans_str(seans_loaded).time_computer = date_2unixtime(_
			seans_str(seans_loaded).seans.Day1, _
			seans_str(seans_loaded).seans.Month1, _
			seans_str(seans_loaded).seans.Year1, _
			seans_str(seans_loaded).seans.Hour1, _
			seans_str(seans_loaded).seans.Minute1, _
			seans_str(seans_loaded).seans.Second1)

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

	DeAllocate (lst)

End Sub


''' =======================================================================


Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Integer
	Return ( CPtr(seans_struct Ptr, elem1) -> time_computer ) - ( CPtr(seans_struct Ptr, elem2) -> time_computer )
End Function


''' =======================================================================
