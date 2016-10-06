
' fbc -gen gcc -r

#Include Once "albom_lib.bi"
#Include Once "albom_log.bi"
#Include Once "albom_version.bi"
#Include  "window9.bi"



''' ===================================

Declare Function ResultsExportTxt(directory_in As String) As Integer
Declare Function ResultsExportVelocityTxt(directory_in As String) As Integer

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
#Define COMBO_TYPE	   14

#Define TEXT_COPYRIGHT  15

Const As String caption = "UPRISE v" + UPRISE_VERSION


''' ===================================

IncludeBinary("images/logo.png", ImageLogo)
IncludeBinary("images/open.png", FolderIcon)
IncludeBinary("images/save.png", SaveIcon)
IncludeBinary("images/lamp.png", HelpIcon)
IncludeBinary("images/tool.png", OptionsIcon)
IncludeBinary("images/search.png", SearchIcon)


''' ===================================

font = Cast(UINT, LoadFont("Verdana", 8))


hwnd = Cast(UINT, OpenWindow(caption,0,0,530,330, WS_VISIBLE Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_SYSMENU))
CenterWindow(CPtr(Any Ptr, hwnd))

ImageGadget(IMG_NAME, 0, 0, 550, 80, Catch_Image(@ImageLogo()))

ButtonImageGadget(BTN_IN,      10,  100, 24, 24, Cast(UINT, Catch_Image(@FolderIcon())))
ButtonImageGadget(BTN_OUT,     40,  100, 24, 24, Cast(UINT, Catch_Image(@SearchIcon())))
ButtonImageGadget(BTN_SAVE,    70,  100, 24, 24, Cast(UINT, Catch_Image(@SaveIcon())))
ButtonImageGadget(BTN_OPTIONS, 100, 100, 24, 24, Cast(UINT, Catch_Image(@OptionsIcon())))
ButtonImageGadget(BTN_HELP,    130, 100, 24, 24, Cast(UINT, Catch_Image(@HelpIcon())))

ButtonGadget(BTN_VIEW,      10, 140, 220, 28, "1. Просмотр данных")
ButtonGadget(BTN_INTEGRATE, 10, 170, 220, 28, "2. Временное усреднение")
ButtonGadget(BTN_APPROX,    10, 200, 220, 28, "3. Высотная коррекция")
ButtonGadget(BTN_ESTIMATE,  10, 230, 220, 28, "4. Оценка параметров")

ComboBoxGadget(COMBO_APPROX,   240, 200, 270, 80)
ComboBoxGadget(COMBO_ESTIMATE, 240, 230, 270, 80)
ComboBoxGadget(COMBO_TYPE,     240, 155, 270, 80)

AddComboBoxItem(COMBO_APPROX, "Трапецеидальное суммирование", -1)
AddComboBoxItem(COMBO_APPROX, "Скользящее окно", -1)
SetItemComboBox(COMBO_APPROX, 0)

AddComboBoxItem(COMBO_ESTIMATE, "Температуры и ионный состав", -1)
AddComboBoxItem(COMBO_ESTIMATE, "Скорость движения плазмы", -1)
SetItemComboBox(COMBO_ESTIMATE, 0)

AddComboBoxItem(COMBO_TYPE, "Файлы SNew (коррелятор K3)", -1)
AddComboBoxItem(COMBO_TYPE, "Файлы SOld (коррелятор K1)", -1)
SetItemComboBox(COMBO_TYPE, 0)

TextGadget (TEXT_COPYRIGHT, 10, 270, 520, 24 ,"© 2013–2014 Богомаз А.В., Котов Д.В. (Институт ионосферы)")


SetGadgetFont(BTN_VIEW,      font)
SetGadgetFont(BTN_SPLINE,    font)
SetGadgetFont(BTN_INTEGRATE, font)
SetGadgetFont(BTN_APPROX,    font)
SetGadgetFont(BTN_ESTIMATE,  font)
SetGadgetFont(BTN_HELP,      font)

SetGadgetFont(COMBO_APPROX,  font)
SetGadgetFont(COMBO_ESTIMATE,font)
SetGadgetFont(COMBO_TYPE,font)

SetGadgetFont(TEXT_COPYRIGHT,font)


Do
	Var event=WaitEvent()

	Select Case event

		Case EventClose
			End

		Case EventGadget
			Select Case EventNumber

				Case BTN_VIEW
					If GetItemComboBox(COMBO_TYPE) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "1_UPRISE_view_SNew.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "1_UPRISE_view_SOld.exe", NULL, NULL, SW_SHOWNORMAL)
					EndIf

				Case BTN_INTEGRATE
					If GetItemComboBox(COMBO_TYPE) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "2_UPRISE_integrate_SNew.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "2_UPRISE_integrate_SOld.exe", NULL, NULL, SW_SHOWNORMAL)
					EndIf

				Case BTN_APPROX
					If GetItemComboBox(COMBO_APPROX) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_approximate_trapezoidal.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_approximate_polynomial.exe", NULL, NULL, SW_SHOWNORMAL)
					EndIf


				Case BTN_ESTIMATE
					If GetItemComboBox(COMBO_ESTIMATE) = 0 Then
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "4_UPRISE_estimate_grad.exe", NULL, NULL, SW_SHOWNORMAL)
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "4_UPRISE_velocity.exe", NULL, NULL, SW_SHOWNORMAL)
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

					Dim As Integer type_of_export = 0

					Var hwnd_export = OpenWindow(caption,0,0,400,200, WS_VISIBLE Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_SYSMENU)
					CenterWindow(hwnd_export)
					Dim As Integer close_=0

					#Define TEXT_MESSAGE 100
					#Define COMBO_FORMAT	101
					#Define BUTTON_EXPORT_CANCEL	102
					#Define BUTTON_EXPORT_OK		103

					Var GagetText = TextGadget(TEXT_MESSAGE,10,10,390,30,"Укажите формат экспорта")
					SetGadgetFont(TEXT_MESSAGE, font)

					Var comboFormat = ComboBoxGadget(COMBO_FORMAT,10,40,380,130)
					SetGadgetFont(COMBO_FORMAT, font)
					AddComboBoxItem(COMBO_FORMAT, "Простой текст",-1)
					AddComboBoxItem(COMBO_FORMAT, "Данные для gnuplot",-1)
					SetItemComboBox(COMBO_FORMAT, 0)

					Var buttonExportOk = ButtonGadget(BUTTON_EXPORT_OK, 260, 130, 100, 30, "Экспорт")
					SetGadgetFont(BUTTON_EXPORT_OK, font)

					Var buttonExportCancel = ButtonGadget(BUTTON_EXPORT_CANCEL, 150, 130, 100, 30, "Отмена")
					SetGadgetFont(BUTTON_EXPORT_CANCEL, font)

					Do
						Var event=WindowEvent()

						Select Case event

							Case EventClose
								FreeGadget(TEXT_MESSAGE)
								FreeGadget(COMBO_FORMAT)
								FreeGadget(BUTTON_EXPORT_OK)
								FreeGadget(BUTTON_EXPORT_CANCEL)
								Close_Window(hwnd_export)
								close_= 1
								type_of_export = 0

							Case eventgadget
								If eventnumber()=BUTTON_EXPORT_CANCEL Then
									type_of_export = 0
									close_= 1
									FreeGadget(TEXT_MESSAGE)
									FreeGadget(COMBO_FORMAT)
									FreeGadget(BUTTON_EXPORT_OK)
									FreeGadget(BUTTON_EXPORT_CANCEL)
									Close_Window(hwnd_export)
								EndIf
								If eventnumber()=BUTTON_EXPORT_OK Then
									close_= 1
									type_of_export = GetItemComboBox(COMBO_FORMAT)+1
									FreeGadget(TEXT_MESSAGE)
									FreeGadget(COMBO_FORMAT)
									FreeGadget(BUTTON_EXPORT_OK)
									FreeGadget(BUTTON_EXPORT_CANCEL)
									Close_Window(hwnd_export)
								EndIf

						End Select
					Loop Until close_= 1

					If type_of_export <> 0 Then

						Dim As String directory
						Dim As Integer result
						directory = ShellFolder("Укажите каталог с результатами (step3)", GetCurentDir()+"\out", BIF_USENEWUI Or BIF_NONEWFOLDERBUTTON)

						If Len(directory) > 0 Then

							Select Case Mid(directory, Len(directory), 1)

								Case "3" ' step3
									result = ResultsExportTxt(directory)

								Case "4" ' step4
									result = ResultsExportVelocityTxt(directory)

							End Select

							If result = EXPORT_OK Then
								MessBox(caption, "Экспорт успешно завершён", MB_ICONINFORMATION)
							Else
								Dim As String ERROR_MESSAGE(1 To 4) => {"Ошибка в файле T.txt", "Ошибка в файле H.txt", "Экспорт отменён", "Ошибка чтения файла" }
								MessBox(caption, ERROR_MESSAGE(result), MB_ICONEXCLAMATION)
							EndIf

						EndIf

					EndIf



			End Select

	End Select

Loop


''' ===================================

Function ResultsExportTxt(directory_in As String) As Integer

	Dim As Integer nH, nT
	Dim As Integer t, h
	Dim As Integer file
	Dim As Double temp

	Dim As String directory_out

	' получаем количество сеансов
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



	' получаем количество высот
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




	' выделяем память
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)
	ReDim As Double  q_array  (0 To nT-1, 0 To nH-1)
	ReDim As Double  ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd_array(0 To nT-1, 0 To nH-1)


	' считываем время сеансов
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t

	Close #file


	' считываем значения высот
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h

	Close #file



	directory_out = ShellFolder("Укажите каталог, в который необходимо сохранить результаты", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf

	/'

	' чтение данных из файла
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



	' запись Ti

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



	' запись Te

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



	' запись He

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




	' запись Hyd

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



	' запись Q

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

'/


	ReDim As Double  _ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _hyd_array(0 To nT-1, 0 To nH-1)

	ReDim As Double  ti2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd2_array(0 To nT-1, 0 To nH-1)

	' чтение данных из файла
	For h = 0 To nH-1

		file = FreeFile()
		Open directory_in+"\Q."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, q_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, hyd_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\He.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, he_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\_Ti.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _ti_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\_Te.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _te_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\_Hyd.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _hyd_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\_He.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, _he_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Ti2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, ti2_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Te2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, te2_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\Hyd2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, hyd2_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\He2.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, he2_array(t, h)
		Next t
		Close #file

	Next h



	' запись Q

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


	' запись Ti

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


	file = FreeFile()
	Open directory_out+"\_Ti.txt" For Output As #file
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
			Print #file, Using "###### "; _ti_array(t, h);
		Next h
	Next t

	Close #file


	file = FreeFile()
	Open directory_out+"\Ti2.txt" For Output As #file
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
			Print #file, Using "###### "; ti2_array(t, h);
		Next h
	Next t

	Close #file


	' запись Te

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

	file = FreeFile()
	Open directory_out+"\_Te.txt" For Output As #file
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
			Print #file, Using "###### "; _te_array(t, h);
		Next h
	Next t

	Close #file

	file = FreeFile()
	Open directory_out+"\Te2.txt" For Output As #file
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
			Print #file, Using "###### "; te2_array(t, h);
		Next h
	Next t

	Close #file

	' запись He

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


	file = FreeFile()
	Open directory_out+"\_He.txt" For Output As #file
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
			Print #file, Using "###.## "; _he_array(t, h);
		Next h
	Next t

	Close #file


	file = FreeFile()
	Open directory_out+"\He2.txt" For Output As #file
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
			Print #file, Using "###.## "; he2_array(t, h);
		Next h
	Next t

	Close #file

	' запись Hyd

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


	file = FreeFile()
	Open directory_out+"\_Hyd.txt" For Output As #file
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
			Print #file, Using "###.## "; _hyd_array(t, h)/2;
		Next h
	Next t

	Close #file


	file = FreeFile()
	Open directory_out+"\Hyd2.txt" For Output As #file
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
			Print #file, Using "###.## "; hyd2_array(t, h)/2;
		Next h
	Next t

	Close #file


	Return EXPORT_OK

End Function

''' ===================================

Function ResultsExportVelocityTxt(directory_in As String) As Integer


	Dim As Integer nH, nT
	Dim As Integer t, h
	Dim As Integer file
	Dim As Double temp

	Dim As String directory_out

	' получаем количество сеансов
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



	' получаем количество высот
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



	directory_out = ShellFolder("Укажите каталог, в который необходимо сохранить результаты", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf



	' выделяем память
	ReDim As Double  t_array  (0 To nT-1)
	ReDim As Integer h_array  (0 To nH-1)
	ReDim As Double  vel_array (0 To nT-1, 0 To nH-1)


	' считываем время сеансов
	file = FreeFile()
	Open directory_in+"\T.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For t = 0 To nT-1
		Input #file, t_array(t)
	Next t

	Close #file


	' считываем значения высот
	file = FreeFile()
	Open directory_in+"\H.txt" For Input As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	For h = 0 To nH-1
		Input #file, h_array(h)
	Next h

	Close #file


	' чтение данных из файла
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



	' запись V

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
