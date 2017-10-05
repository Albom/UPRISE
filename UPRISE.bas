
' fbc -gen gcc -r

#Include Once "albom_lib.bi"
#Include Once "albom_log.bi"
#Include Once "albom_version.bi"
#Include  "window9.bi"



''' ===================================

Declare Function ResultsExportTxt(directory_in As String) As Integer
Declare Function ResultsExportGp(directory_in As String) As Integer
Declare Function ResultsExportKentavr(directory_in As String) As Integer
Declare Function ResultsExportVelocityTxt(directory_in As String) As Integer
Declare Function numToHexString(n As Integer) As String

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
#Define BTN_ESTIMATE		5
#Define BTN_HELP			6
#Define BTN_IN			   7
#Define BTN_OUT         8
#Define BTN_SAVE		   9
#Define BTN_OPTIONS     10

#Define COMBO_ESTIMATE  11
#Define COMBO_TYPE	   12

#Define TEXT_COPYRIGHT  13

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


hwnd = Cast(UINT, OpenWindow(caption,0,0,530,300, WS_VISIBLE Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_SYSMENU))
CenterWindow(CPtr(Any Ptr, hwnd))


ImageGadget(IMG_NAME, 0, 0, 550, 80, Catch_Image( ImageLogo()) )

ButtonImageGadget(BTN_IN,      10,  100, 24, 24, Catch_Image(FolderIcon()))
ButtonImageGadget(BTN_OUT,     40,  100, 24, 24, Catch_Image(SearchIcon()))
ButtonImageGadget(BTN_SAVE,    70,  100, 24, 24, Catch_Image(SaveIcon()))
ButtonImageGadget(BTN_OPTIONS, 100, 100, 24, 24, Catch_Image(OptionsIcon()))
ButtonImageGadget(BTN_HELP,    130, 100, 24, 24, Catch_Image(HelpIcon()))

GadgetToolTip(BTN_IN, "Открыть папку с исходными данными")
GadgetToolTip(BTN_OUT, "Открыть папку с результатами обработки")
GadgetToolTip(BTN_SAVE, "Экспортировать данные")
GadgetToolTip(BTN_OPTIONS, "Открыть конфигурационный файл в текстовом редакторе")
GadgetToolTip(BTN_HELP, "Помощь")

ButtonGadget(BTN_VIEW,      10, 140, 220, 28, "1. Просмотр данных")
ButtonGadget(BTN_INTEGRATE, 10, 170, 220, 28, "2. Обработка данных")
ButtonGadget(BTN_ESTIMATE,  10, 200, 220, 28, "3. Оценка параметров")

GadgetToolTip(BTN_VIEW, "Запустить программу просмотра и фильтрации данных")
GadgetToolTip(BTN_INTEGRATE, "Запустить программу обработки данных")
GadgetToolTip(BTN_ESTIMATE, "Запустить программу оценки параметров ионосферы")

ComboBoxGadget(COMBO_TYPE,     240, 140, 270, 80)
ComboBoxGadget(COMBO_ESTIMATE, 240, 200, 270, 80)

GadgetToolTip(COMBO_TYPE, "Выбор типа данных")
GadgetToolTip(COMBO_ESTIMATE, "Выбор типа параметров")

AddComboBoxItem(COMBO_ESTIMATE, "Температуры и ионный состав", -1)
AddComboBoxItem(COMBO_ESTIMATE, "Скорость движения плазмы", -1)
AddComboBoxItem(COMBO_ESTIMATE, "Wave Edition", -1)
SetItemComboBox(COMBO_ESTIMATE, 0)

AddComboBoxItem(COMBO_TYPE, "Файлы SNew (коррелятор K3)", -1)
AddComboBoxItem(COMBO_TYPE, "Файлы SOld (коррелятор K1)", -1)
AddComboBoxItem(COMBO_TYPE, "Файлы COld (4-й режим)", -1)
SetItemComboBox(COMBO_TYPE, 0)

TextGadget (TEXT_COPYRIGHT, 10, 240, 520, 24 ,"© 2013–2017 Богомаз А.В., Котов Д.В. (Институт ионосферы)")


SetGadgetFont(BTN_VIEW,      font)
SetGadgetFont(BTN_SPLINE,    font)
SetGadgetFont(BTN_INTEGRATE, font)
SetGadgetFont(BTN_ESTIMATE,  font)
SetGadgetFont(BTN_HELP,      font)

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
					Select Case GetItemComboBox(COMBO_TYPE)
						Case 0
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "1_UPRISE_view_SNew.exe", NULL, NULL, SW_SHOWNORMAL)
						Case 1
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "1_UPRISE_view_SOld.exe", NULL, NULL, SW_SHOWNORMAL)
						Case 2
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "1_UPRISE_view_COld.exe", NULL, NULL, SW_SHOWNORMAL)
					End Select

				Case BTN_INTEGRATE
					Select Case GetItemComboBox(COMBO_TYPE)
						Case 0
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "2_UPRISE_processing_SNew.exe", NULL, NULL, SW_SHOWNORMAL)
						Case 1
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "2_UPRISE_processing_SOld.exe", NULL, NULL, SW_SHOWNORMAL)
						Case 2
							ShellExecute(CPtr(Any Ptr, hwnd), "open", "2_UPRISE_processing_COld.exe", NULL, NULL, SW_SHOWNORMAL)
					End Select


				Case BTN_ESTIMATE
					If GetItemComboBox(COMBO_TYPE) = 0 Or  GetItemComboBox(COMBO_TYPE) = 1  Then
						Select Case GetItemComboBox(COMBO_ESTIMATE)
							Case 0
								ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_estimate.exe", NULL, NULL, SW_SHOWNORMAL)
							Case 1
								ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_velocity.exe", NULL, NULL, SW_SHOWNORMAL)
							Case 2
								ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_estimate_wave.exe", NULL, NULL, SW_SHOWNORMAL)
						End Select
					Else
						ShellExecute(CPtr(Any Ptr, hwnd), "open", "3_UPRISE_estimate_COld.exe", NULL, NULL, SW_SHOWNORMAL)
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
					/'
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
					AddComboBoxItem(COMBO_FORMAT, "SS-файлы",-1)
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
					'/

					type_of_export = 1
					If type_of_export <> 0 Then

						Dim As String directory
						Dim As Integer result
						directory = ShellFolder("Укажите каталог с результатами:" + Chr(10)+ "step3 - для экспорта температур и ионного состава;"+ Chr(10)+"step4 - для экспорта скорости движения.", GetCurentDir()+"\out", BIF_USENEWUI Or BIF_NONEWFOLDERBUTTON)

						If Len(directory) > 0 Then

							Select Case Mid(directory, Len(directory), 1)

								Case "3" ' step3

									Select Case type_of_export
										Case 1
											'MessBox(caption, "1", MB_ICONINFORMATION)
											result = ResultsExportTxt(directory)
										Case 2
											'MessBox(caption, "2", MB_ICONINFORMATION)
											result = ResultsExportGp(directory)
										Case 3
											'MessBox(caption, "SS", MB_ICONINFORMATION)
											result = ResultsExportKentavr(directory)
									End Select


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
	ReDim As Double  m_array  (0 To nT-1, 0 To nH-1)
	ReDim As Double  o_array  (0 To nT-1, 0 To nH-1)


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


	For i As Integer = 0 To nH-1
		For j As Integer = 0 To nH-1-1
			If h_array(j) > h_array(j+1) Then
				Dim As Integer tmp = h_array(j)
				h_array(j) = h_array(j+1)
				h_array(j+1) = tmp
			EndIf
		Next j
	Next i



	directory_out = ShellFolder("Укажите каталог, в который необходимо сохранить результаты", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf



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
		Open directory_in+"\M.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, m_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\O.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, o_array(t, h)
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




	' запись Qh2

	file = FreeFile()
	Open directory_out+"\Qh2.txt" For Output As #file
	If Err() <> 0 Then
		Return EXPORT_ERROR_OPEN_FILE
	EndIf

	Print #file, "      0 ";

	For h = 0 To nH-1
		Print #file, Using "    ###### "; h_array(h);
	Next h

	For t = 0 To nT-1
		Print #file,
		Print #file, Using "##.#### "; t_array(t);
		For h = 0 To nH-1
			Print #file, Using "##.###^^^^ "; q_array(t, h)*(h_array(h)^2);
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



	' запись M

	file = FreeFile()
	Open directory_out+"\M.txt" For Output As #file
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
			Print #file, Using "###.## "; m_array(t, h);
		Next h
	Next t

	Close #file


	' запись O

	file = FreeFile()
	Open directory_out+"\O.txt" For Output As #file
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
			Print #file, Using "###.## "; o_array(t, h);
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





Function ResultsExportGp(directory_in As String) As Integer
	Return EXPORT_OK
End Function



Function ResultsExportKentavr(directory_in As String) As Integer

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
	ReDim As Double  m_array  (0 To nT-1, 0 To nH-1)
	ReDim As Double  o_array  (0 To nT-1, 0 To nH-1)


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


	For i As Integer = 0 To nH-1
		For j As Integer = 0 To nH-1-1
			If h_array(j) > h_array(j+1) Then
				Dim As Integer tmp = h_array(j)
				h_array(j) = h_array(j+1)
				h_array(j+1) = tmp
			EndIf
		Next j
	Next i



	directory_out = ShellFolder("Укажите каталог, в который необходимо сохранить результаты", "", BIF_USENEWUI)
	If Len(directory_out) < 1 Then
		Return EXPORT_ERROR_ABORT
	EndIf



	ReDim As Double  _ti_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _te_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _he_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  _hyd_array(0 To nT-1, 0 To nH-1)

	ReDim As Double  ti2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  te2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  he2_array (0 To nT-1, 0 To nH-1)
	ReDim As Double  hyd2_array(0 To nT-1, 0 To nH-1)

	ReDim As Double  fkr_array(0 To nT-1)
	ReDim As Double  pn_array(0 To nT-1)

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
		Open directory_in+"\M.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, m_array(t, h)
		Next t
		Close #file

		file = FreeFile()
		Open directory_in+"\O.-1."+ Str(h_array(h)) +".txt" For Input As #file
		For t = 0 To nT-1
			Input #file, o_array(t, h)
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




	ReDim As String head(0 To nT-1)
	file = FreeFile()
	Open directory_in+"\HEADER.txt" For Input As #file
	For t = 0 To nT-1
		Input #file, head(t)
	Next t
	Close #file


	file = FreeFile()
	Open directory_in+"\Fkr.txt" For Input As #file
	For t = 0 To nT-1
		Input #file, fkr_array(t)
	Next t
	Close #file


	file = FreeFile()
	Open directory_in+"\Pn.txt" For Input As #file
	For t = 0 To nT-1
		Input #file, pn_array(t)
	Next t
	Close #file


	' Запись файлов

	For t = 0 To nT-1
		file = FreeFile()
		Open directory_out+"\SS"+ Mid(head(t), 1, 2) + Mid(head(t), 4, 2) + Mid(head(t), 9, 2) + "." + numToHexString(t)  For Output As #file
		Print #file, head(t);
		Print #file, "   Fkr = "; fkr_array(t);
		Print #file, "   Pn = "; pn_array(t)
		Print #file, "Hmax(Ne)=";
		Close #file
	Next t


	Return EXPORT_OK
End Function



Function numToHexString(n As Integer) As String

	Dim As String s
	Dim As Integer c
	Dim As String t(0 To 15) => {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"}

	For i As Integer = 0 To 2
		c = n Mod 16
		n \= 16
		s = t(c) + s
	Next

	Return s

End Function

