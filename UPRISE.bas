
' fbc -gen gcc -r

#Include Once "albom_lib.bi"
#Include Once "albom_log.bi"
#Include  "window9.bi"



''' ===================================

Declare Function ResultsExport(directory_in As String) As Integer
Declare Function ResultsExportVelocity(directory_in As String) As Integer

''' ===================================

Dim As Integer hwnd
Dim As Integer font

''' ===================================

#Define EXPORT_OK						0
#Define EXPORT_ERROR_nT				1
#Define EXPORT_ERROR_nH				2
#Define EXPORT_ERROR_ABORT			3
#Define EXPORT_ERROR_OPEN_FILE	4



#Define IMG_NAME 			1

#Define BTN_VIEW 			2
#Define BTN_SPLINE		3
#Define BTN_INTEGRATE	4
#Define BTN_APPROX		5
#Define BTN_ESTIMATE		6
#Define BTN_HELP			7
#Define BTN_IN			   8
#Define BTN_OUT         9
#Define BTN_SAVE		   10
#Define BTN_OPTIONS     11

#Define COMBO_APPROX    12
#Define COMBO_ESTIMATE  13


''' ===================================

IncludeBinary("images/logo2.png", ImageLogo)
IncludeBinary("images/open.png", FolderIcon)
IncludeBinary("images/save.png", SaveIcon)
IncludeBinary("images/lamp.png", HelpIcon)
IncludeBinary("images/tool.png", OptionsIcon)
IncludeBinary("images/search.png", SearchIcon)


''' ===================================

font = Cast(UINT, LoadFont("Verdana", 9))


hwnd = Cast(UINT, OpenWindow("UPRISE v1.0 beta",0,0,550,340, WS_VISIBLE Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_SYSMENU))
CenterWindow(CPtr(Any Ptr, hwnd))

ImageGadget(IMG_NAME, 0, 0, 550, 80, Catch_Image(@ImageLogo()))

ButtonImageGadget(BTN_IN,      10,  100, 24, 24, Cast(UINT, Catch_Image(@FolderIcon())))
ButtonImageGadget(BTN_OUT,     40,  100, 24, 24, Cast(UINT, Catch_Image(@SearchIcon())))
ButtonImageGadget(BTN_SAVE,    70,  100, 24, 24, Cast(UINT, Catch_Image(@SaveIcon())))
ButtonImageGadget(BTN_OPTIONS, 100, 100, 24, 24, Cast(UINT, Catch_Image(@OptionsIcon())))
ButtonImageGadget(BTN_HELP,    130, 100, 24, 24, Cast(UINT, Catch_Image(@HelpIcon())))

ButtonGadget(BTN_VIEW,      10, 140, 220, 28, "1. �������� ������")
ButtonGadget(BTN_SPLINE,    10, 170, 220, 28, "2. ���������� ���������")
ButtonGadget(BTN_INTEGRATE, 10, 200, 220, 28, "3. ��������� ����������")
ButtonGadget(BTN_APPROX,    10, 230, 220, 28, "4. �������� ���������")
ButtonGadget(BTN_ESTIMATE,  10, 260, 220, 28, "5. ������ ����������")

ComboBoxGadget(COMBO_APPROX,   240, 230, 270, 80)
ComboBoxGadget(COMBO_ESTIMATE, 240, 260, 270, 80)

AddComboBoxItem(COMBO_APPROX, "���������� ����", -1)
AddComboBoxItem(COMBO_APPROX, "��������������� ������������", -1)
SetItemComboBox(COMBO_APPROX, 0)


AddComboBoxItem(COMBO_ESTIMATE, "����������� � ������ ������", -1)
AddComboBoxItem(COMBO_ESTIMATE, "�������� �������� ������", -1)
SetItemComboBox(COMBO_ESTIMATE, 0)

SetGadgetFont(BTN_VIEW,      font)
SetGadgetFont(BTN_SPLINE,    font)
SetGadgetFont(BTN_INTEGRATE, font)
SetGadgetFont(BTN_APPROX,    font)
SetGadgetFont(BTN_ESTIMATE,  font)
SetGadgetFont(BTN_HELP,      font)

SetGadgetFont(COMBO_APPROX,  font)
SetGadgetFont(COMBO_ESTIMATE,font)


Do
	Var event=WaitEvent()

	Select Case event

		Case EventClose
			End

		Case EventGadget
			Select Case EventNumber

				Case BTN_VIEW
					ShellExecute(CPtr(Any Ptr, hwnd), "open", "01_UPRISE_view.exe", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_SPLINE
					ShellExecute(CPtr(Any Ptr, hwnd), "open", "02_UPRISE_spline.exe", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_INTEGRATE
					ShellExecute(CPtr(Any Ptr, hwnd), "open", "03_UPRISE_integrate.exe", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_APPROX
					If GetItemComboBox(COMBO_APPROX) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "04_UPRISE_approximate_polynomial.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "04_UPRISE_approximate_trapezoidal.exe", NULL, NULL, SW_SHOWNORMAL)
					EndIf


				Case BTN_ESTIMATE
					If GetItemComboBox(COMBO_ESTIMATE) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "05_UPRISE_estimate.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "05_UPRISE_velocity.exe", NULL, NULL, SW_SHOWNORMAL)
					EndIf


				Case BTN_HELP
					ShellExecute(CPtr(Any Ptr, hwnd), "open", "Help.chm", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_IN
					ShellExecute(CPtr(Any Ptr, hwnd), "explore", "in", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_OUT
					ShellExecute(CPtr(Any Ptr, hwnd), "explore", "out", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_Options
					ShellExecute(CPtr(Any Ptr, hwnd), "open", "config.dat", NULL, NULL, SW_SHOWNORMAL)

				Case BTN_SAVE
					/'
					Dim As Integer type_of_export = 0

					Var hwnd_export = OpenWindow("UPRISE v1.0 beta Export",0,0,550,340, WS_VISIBLE Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_SYSMENU)
					CenterWindow(hwnd_export)
					Dim As Integer close_=0

					#Define TEXT_MESSAGE 100

					Var GagetText = TextGadget(TEXT_MESSAGE,10,10,540,30,"������� ������ ��������")
					SetGadgetFont(TEXT_MESSAGE, font)

					Do
						Var event=WindowEvent()

						Select Case event

							Case EventClose
								FreeGadget(TEXT_MESSAGE)
								Close_Window(hwnd_export)
								close_= 1
								type_of_export = 0

						End Select
					Loop Until close_= 1

					If type_of_export <> 0 Then
				'/
					Dim As String directory
					Dim As Integer result
					directory = ShellFolder("������� ������� � ������������ (step4 ��� step5)", GetCurentDir()+"\out", BIF_USENEWUI Or BIF_NONEWFOLDERBUTTON)

					If Len(directory) > 0 Then

						Select Case Mid(directory, Len(directory), 1)

							Case "4" ' step4
								result = ResultsExport(directory)

							Case "5" ' step5
								result = ResultsExportVelocity(directory)

						End Select

						If result = EXPORT_OK Then
							MessBox("UPRISE", "������� ������� ��������", MB_ICONINFORMATION)
						Else 
							Dim As String ERROR_MESSAGE(1 To 4) => {"������ � ����� T.txt", "������ � ����� H.txt", "������� �������", "������ ������ �����" }
							MessBox("UPRISE", ERROR_MESSAGE(result), MB_ICONEXCLAMATION)
						EndIf

					EndIf

					'		EndIf



			End Select

	End Select

Loop


''' ===================================

Function ResultsExport(directory_in As String) As Integer

	Dim As Integer nH, nT
	Dim As Integer t, h
	Dim As Integer file
	Dim As Double temp

	Dim As String directory_out

	' �������� ���������� �������
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	nT = 0
	Do While Not Eof(file)
		Input #file, temp
		nT += 1
	Loop

	Close #file

	If nT < 1 Then
		Return EXPORT_ERROR_nT
	EndIf



	' �������� ���������� �����
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	nH = 0
	Do While Not Eof(file)
		Input #file, temp
		nH += 1
	Loop

	Close #file

	If nH < 1 Then
		Return EXPORT_ERROR_nH
	EndIf




	' �������� ������
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)
	ReDim As Double  q_array  (0 To nT-1, 0 To nH-1)
	ReDim As Double  ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd_array(0 To nT-1, 0 To nH-1)


	' ��������� ����� �������
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t

	Close #file


	' ��������� �������� �����
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h

	Close #file



	directory_out = ShellFolder("������� �������, � ������� ���������� ��������� ����������", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf



	' ������ ������ �� �����
	For h = 0 To nH-1


		file = FreeFile()
		Open directory_in+"\Ti2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Open directory_in+"\Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
			If Err() <> 0 Then
				Return EXPORT_ERROR_OPEN_FILE
			EndIf
		EndIf

		For t = 0 To nT-1
			Input #file, ti_array(t, h)
		Next t

		Close #file


		file = FreeFile()
		Open directory_in+"\Te2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Open directory_in+"\Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
			If Err() <> 0 Then
				Return EXPORT_ERROR_OPEN_FILE
			EndIf
		EndIf

		For t = 0 To nT-1
			Input #file, te_array(t, h)
		Next t

		Close #file


		file = FreeFile()
		Open directory_in+"\Hyd2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Open directory_in+"\Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
			If Err() <> 0 Then
				Return EXPORT_ERROR_OPEN_FILE
			EndIf
		EndIf

		For t = 0 To nT-1
			Input #file, hyd_array(t, h)
		Next t

		Close #file


		file = FreeFile()
		Open directory_in+"\He2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Open directory_in+"\He.-1."+ Str(h_array(h)) +".txt" For Input As #file
			If Err() <> 0 Then
				Return EXPORT_ERROR_OPEN_FILE
			EndIf
		EndIf

		For t = 0 To nT-1
			Input #file, he_array(t, h)
		Next t

		Close #file



		file = FreeFile()
		Open directory_in+"\Q."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Return EXPORT_ERROR_OPEN_FILE
		EndIf

		For t = 0 To nT-1
			Input #file, q_array(t, h)
		Next t

		Close #file


	Next h



	' ������ Ti

	file = FreeFile()
	Open directory_out+"\Ti.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

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



	' ������ Te

	file = FreeFile()
	Open directory_out+"\Te.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

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



	' ������ He

	file = FreeFile()
	Open directory_out+"\He.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

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




	' ������ Hyd

	file = FreeFile()
	Open directory_out+"\Hyd.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

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



	' ������ Q

	file = FreeFile()
	Open directory_out+"\Q.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "###.## "; q_array(t, h);
		Next h
	Next t

	Close #file




	Return EXPORT_OK

End Function

''' ===================================

Function ResultsExportVelocity(directory_in As String) As Integer


	Dim As Integer nH, nT
	Dim As Integer t, h
	Dim As Integer file
	Dim As Double temp

	Dim As String directory_out

	' �������� ���������� �������
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	nT = 0
	Do While Not Eof(file)
		Input #file, temp
		nT += 1
	Loop

	Close #file

	If nT < 1 Then
		Return EXPORT_ERROR_nT
	EndIf



	' �������� ���������� �����
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	nH = 0
	Do While Not Eof(file)
		Input #file, temp
		nH += 1
	Loop

	Close #file

	If nH < 1 Then
		Return EXPORT_ERROR_nH
	EndIf



	directory_out = ShellFolder("������� �������, � ������� ���������� ��������� ����������", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf



	' �������� ������
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)
	ReDim As Double  vel_array (0 To nT-1, 0 To nH-1)


	' ��������� ����� �������
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t

	Close #file


	' ��������� �������� �����
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h

	Close #file


	' ������ ������ �� �����
	For h = 0 To nH-1


		file = FreeFile()
		Open directory_in+"\V."+ Str(h_array(h)) +".txt" For Input As #file
		If Err() <> 0 Then
			Return EXPORT_ERROR_OPEN_FILE
		EndIf

		For t = 0 To nT-1
			Input #file, vel_array(t, h)
		Next t

		Close #file

	Next h



	' ������ V

	file = FreeFile()
	Open directory_out+"\V.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "########  "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "#####.##  "; vel_array(t, h);
		Next h
	Next t

	Close #file


	Return EXPORT_OK

End Function