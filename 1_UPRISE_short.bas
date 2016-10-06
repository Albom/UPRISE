
#Include Once "albom_lib.bi"	'Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		'Подключение лога
#Include "fbgfx.bi"			'Подключение графической библиотеки
#Include Once "albom_version.bi"

#Include  "crt/stdlib.bi"


#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf

Type seans_struct
	Dim filename_full As ZString*256
	Dim filename As ZString*64
	Dim p1(0 To 679) As Double
	Dim p2(0 To 679) As Double
	Dim p(0 To 679)  As Double
	Dim m1(0 To 679) As Integer
	Dim m2(0 To 679) As Integer
	Dim time_decimal As Double
	Dim time_computer As Integer
End Type

Type seans_struct_out
	Dim p(0 To 679)  As Double
	Dim pn  As Double
	Dim time_decimal As Double
End Type

Dim filename As ZString*256 = ""
Dim filename_full As ZString*512 = ""
Dim directory As String
Dim As Integer i, j, k
Dim is1s As Integer
Dim As String SEANS_DIR = "./in/"
Dim As String SEANS_DIR_OUT = "./out/"
Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim As String DirectoryOutput

Dim Shared timestr As ZString*16 ' переменная для представления времени в виде строки

ReDim Preserve Shared As seans_struct seans_str(0 To 1)
Dim Shared seans_loaded As Integer   ' сеансов загружено


Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Integer
Declare Sub Vis_array_load()
Declare Sub Filter(wnd_width As Integer, lev As Double, direction As Integer)

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim Shared As Double razr(0 To 350)

Dim As Integer file, t, h
Dim As Integer seans_num_out = 0


Dim As String razr_filename
Dim As Integer h_start, h_end, h_step
Dim As Integer is_clear = 1
Dim As Integer wnd_width = 15
Dim As Integer n2 = 20
Dim As Double lev = 4.5
Dim As Integer tnak = 15
Dim As Integer tstep = 1

Dim Shared As Integer hCur = 55
Dim Shared As Integer position = 0

Dim As Integer START_X = 0
Dim As Integer DX = 1 ' масштаб по оси x
Dim As Integer DY = 200 ' масштаб по оси y
Dim As Integer Y0 = 250 ' начальное значение по оси y

ReDim Shared vis_array(0 To 1, 0 To 1) As Double ' буфер для отображения графиков
ReDim Shared vis_array_alt(0 To 1, 0 To 679) As Double ' буфер для отображения графиков

''' =======================================================================


SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1

file = FreeFile()
Open "config_short.dat" For Input As #file
Input #file, razr_filename
Input #file, h_start
Input #file, h_end
Input #file, h_step
Input #file, is_clear
Input #file, wnd_width
Input #file, lev
Input #file, n2
Input #file, tnak
Input #file, tstep
Close #file

If razr_load(razr_filename, @razr(0), 330) = 0 Then
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf


Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Short - программа для работы с данными, полученными по короткому импульсу"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

Print "Загрузка... ";

seans_loaded = 0

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

		LoadFiles(SEANS_DIR+directory)

		date_next(@d_day, @d_month, @d_year)

		If i = 0 Then
			DirectoryOutput += directory + "-"
		EndIf

	EndIf

Next i


If seans_loaded = 0 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf


DirectoryOutput += directory
MkDir(SEANS_DIR_OUT +DirectoryOutput)
MkDir(SEANS_DIR_OUT +DirectoryOutput+"/step3")


Print "OK"

Print #1, Str(seans_loaded)+" files loaded"


' Сортировка сеансов по времени
Print "Сортировка по времени... ";
qsort(@seans_str(0), seans_loaded, SizeOf(seans_struct), @seans_struct_time_compare)
Print "OK"





ReDim vis_array(0 To 1, 0 To seans_loaded) As Double ' буфер для отображения графиков


Vis_array_load()

Do

	ScreenLock

	Cls

	If seans_str(position).m1(hCur) = 0 And seans_str(position).m2(hCur) = 0 Then
		Line (5+(position-START_X)*DX, 25)-(5+(position-START_X)*DX, Y0), 7
	Else
		Line (5+(position-START_X)*DX, 25)-(5+(position-START_X)*DX, Y0), 7, , &b0000000011110000
	End If

	Line(0, Y0)-(1023, Y0), 15, , &b0001000100010001

	For i = START_X To (seans_loaded-2)

		If seans_str(i).m1(hCur) <> 0 Then
			Line(5+(i-START_X)*DX, Y0-DY*(vis_array(0, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(0, i+1))),  15, , &b0001000100010001
		Else
			If seans_str(i+1).m1(hCur) <> 0 Then
				Line(5+(i-START_X)*DX, Y0-DY*(vis_array(0, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(0, i+1))), 15, , &b0001000100010001
			Else
				Line(5+(i-START_X)*DX, Y0-DY*(vis_array(0, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(0, i+1))),  15'+1
			End If
		End If


		If seans_str(i).m2(hCur) <> 0 Then
			Line(5+(i-START_X)*DX, Y0-DY*(vis_array(1, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(1, i+1))),  14, , &b0001000100010001
		Else
			If seans_str(i+1).m2(hCur) <> 0 Then
				Line(5+(i-START_X)*DX, Y0-DY*(vis_array(1, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(1, i+1))), 14, , &b0001000100010001
			Else
				Line(5+(i-START_X)*DX, Y0-DY*(vis_array(1, i)))-(5+(i+1-START_X)*DX, Y0-DY*(vis_array(1, i+1))),  14'+1
			End If
		End If


	Next i


	For i = 55 To 679-1
		If seans_str(position).m1(i) <> 0 Then
			Line(5+i-55, 600-DY*vis_array_alt(0, i))-(5+i+1-55, 600-DY*vis_array_alt(0, i+1)), 15, , &b0001000100010001
		Else
			If seans_str(position).m1(i+1) <> 0 Then
				Line(5+i-55, 600-DY*vis_array_alt(0, i))-(5+i+1-55, 600-DY*vis_array_alt(0, i+1)), 15, , &b0001000100010001
			Else
				Line(5+i-55, 600-DY*vis_array_alt(0, i))-(5+i+1-55, 600-DY*vis_array_alt(0, i+1)), 15
			EndIf
		EndIf
	Next


	For i = 55 To 679-1
		If seans_str(position).m2(i) <> 0 Then
			Line(5+i-55, 600-DY*vis_array_alt(1, i))-(5+i+1-55, 600-DY*vis_array_alt(1, i+1)), 14, , &b0001000100010001
		Else
			If seans_str(position).m2(i+1) <> 0 Then
				Line(5+i-55, 600-DY*vis_array_alt(1, i))-(5+i+1-55, 600-DY*vis_array_alt(1, i+1)), 14, , &b0001000100010001
			Else
				Line(5+i-55, 600-DY*vis_array_alt(1, i))-(5+i+1-55, 600-DY*vis_array_alt(1, i+1)), 14
			EndIf
		EndIf
	Next


	If seans_str(position).m1(hCur) = 0 Then
		Line(5+hCur-55, 765)-(5+hCur-55, 300), 15
	Else
		Line(5+hCur-55, 765)-(5+hCur-55, 300), 15, , &b0000000011110000
	EndIf

	Color 14
	Print Using "Время: ###.###  "; seans_str(position).time_decimal;

	Color 10
	Print Using "Высота: № ###"; hCur+1;
	Print Using "  #### км"; seans2_altS(hCur);
	Print ,,

	Color 15
	Print "W - white  ";
	Color 14
	Print "Y - yellow",

	Color 10
	Print "Ctrl+N: Продолжить обработку   ";
	Color 12
	Print "Ctrl+Q: Выход"

	Color 15

	ScreenUnLock

	Dim key As Integer

	key = GetKey()

	If key = KEY_CTRL_N Then Exit Do End If

	If key = KEY_CTRL_Q Then End End If

	Select Case key


		Case KEY_W
			If seans_str(position).m1(hCur) = 0 Then
				seans_str(position).m1(hCur) = 1
			Else
				seans_str(position).m1(hCur) = 0
			EndIf
			Vis_array_load()

		Case KEY_Y
			If seans_str(position).m2(hCur) = 0 Then
				seans_str(position).m2(hCur) = 1
			Else
				seans_str(position).m2(hCur) = 0
			EndIf
			Vis_array_load()


		Case KEY_A, KEY_A_CAPITAL

			Dim As Integer mode

			Color 15
			Print "Автоматическая фильтрация когерентных отражений"
			Print
			Print "Оптимальные параметры: ширина окна - 15, уровень - 4.5"
			Print
			Print "0: Выход"
			Print "1: Удалить все метки"
			Print "2: Проверка предыдущей точки"
			Print "3: Проверка следующей точки"
			Input "[0..3]: ", mode
			Print
			Select Case mode
				Case 2
					Input "Ширина окна: ", wnd_width
					Input "Уровень: ", lev
					Filter(wnd_width, lev, 0)

				Case 3
					Input "Ширина окна: ", wnd_width
					Input "Уровень: ", lev
					Filter(wnd_width, lev, 1)
			End Select
			Vis_array_load()
			/'
		Case KEY_HOME
			position=START_X

		Case KEY_END
			If START_X+(1024\DX)-1 < seans_loaded Then
				position += 1024\DX-1
			Else
				position=seans_loaded-1
			End If
'/

		Case KEY_RIGHT
			If position < seans_loaded-1 Then position += 1 End If
			Vis_array_load() ' загрузить данные для отображения

		Case KEY_LEFT
			If position > 0 Then position -= 1  End If
			Vis_array_load() ' загрузить данные для отображения

		Case KEY_DOWN
			If START_X < seans_loaded-1 Then START_X += 1 End If

		Case KEY_UP
			If START_X > 0  Then START_X -= 1  End If

		Case KEY_PLUS
			If DX < 32 Then DX = DX*2 End If

		Case KEY_MINUS
			If DX > 1 Then DX = DX/2 End If
		Case KEY_PAGE_UP
			If hCur < 679 Then hCur += 1 End If
			Vis_array_load() ' загрузить данные для отображения

		Case KEY_PAGE_DOWN
			If hCur > 0 Then hCur -= 1 End If
			Vis_array_load() ' загрузить данные для отображения

	End Select


Loop

Cls

Print "Интерполяция данных... ";
' интрерполяция

For h = 0 To 679
	For t = 0 To seans_loaded-1
		If seans_str(t).m1(h) = 1 Then

			Dim As Integer c = 0
			Dim As Double d = 0

			' left
			i = 0
			Do While (c < n2\2)

				If t-i < 0 Then
					Exit Do
				EndIf

				If seans_str(t-i).m1(h) = 0 Then
					c += 1
					d += seans_str(t-i).p1(h)
				EndIf

				i += 1

			Loop

			' right
			i = 0
			Do While (c < n2\2)

				If t+i > seans_loaded-1 Then
					Exit Do
				EndIf

				If seans_str(t+i).m1(h) = 0 Then
					c += 1
					d += seans_str(t+i).p1(h)
				EndIf

				i += 1

			Loop

			If c <> 0 Then
				seans_str(t).p1(h) = d/c
			Else
				seans_str(t).p1(h) = 0
			EndIf

		EndIf
	Next t
Next h


For h = 0 To 679
	For t = 0 To seans_loaded-1
		If seans_str(t).m2(h) = 1 Then

			Dim As Integer c = 0
			Dim As Double d = 0

			' left
			i = 0
			Do While (c < n2\2)

				If t-i < 0 Then
					Exit Do
				EndIf

				If seans_str(t-i).m2(h) = 0 Then
					c += 1
					d += seans_str(t-i).p2(h)
				EndIf

				i += 1

			Loop

			' right
			i = 0
			Do While (c < n2\2)

				If t+i > seans_loaded-1 Then
					Exit Do
				EndIf

				If seans_str(t+i).m2(h) = 0 Then
					c += 1
					d += seans_str(t+i).p2(h)
				EndIf

				i += 1

			Loop

			If c <> 0 Then
				seans_str(t).p2(h) = d/c
			Else
				seans_str(t).p2(h) = 0
			EndIf

		EndIf
	Next t
Next h



For t = 0 To seans_loaded-1
	For h = 0 To 679
		seans_str(t).p(h) = seans_str(t).p1(h) + seans_str(t).p2(h)
	Next h
Next t

Print "OK"

Print "Накопление данных по времени... ";

' накопление по времени
ReDim As seans_struct_out seans_str_out(0 To seans_loaded-1)
Dim As Integer seans_current

seans_current = 0
t = 0
Do Until t + tNak > seans_loaded-1

	For h = 0 To 679

		seans_str_out(seans_current).p(h) = 0
		For i = 0 To tNak-1
			seans_str_out(seans_current).p(h) += seans_str(t+i).p(h)
		Next i

		seans_str_out(seans_current).time_decimal = seans_str(t+tNak\2).time_decimal

	Next h

	t += tStep
	seans_current += 1
Loop

Print "OK"

Print "Учёт характеристики разрядника... ";

' учёт разрядника

For t = 0 To seans_current-1
	For h = 0 To 300

		If razr(h) > 0.001 Then
			seans_str_out(t).p(h) /= razr(h)
		Else
			seans_str_out(t).p(h) = 0
		EndIf

	Next h
Next t


Print "OK"
Print "Расчёт мощности НР сигнала... ";


' расчёт мощности шума
For t = 0 To seans_current-1
	seans_str_out(t).pn = 0
	For h = 500 To 599
		seans_str_out(t).pn += seans_str_out(t).p(h)
	Next h
	seans_str_out(t).pn /= 100
Next t

For t = 0 To seans_current-1
	For h = 0 To 679
		seans_str_out(t).p(h) -= seans_str_out(t).pn
	Next h
Next t


For t = 0 To seans_current-1
	For h = 0 To 679
		seans_str_out(t).p(h) /= seans_str_out(t).pn
	Next h
Next t


Print "OK"



file = FreeFile()
Open SEANS_DIR_OUT +DirectoryOutput+"/step3/Short.txt" For Output As #file

Print "Вывод результатов в файл... ";

Print #file, "       0";
For h = h_start To h_end Step h_step
	Print #file, Using "        ####"; seans2_altS(h);
Next h
Print #file,

For t = 0 To seans_current-1
	Print #file, Using " ##.####"; seans_str_out(t).time_decimal;
	For h = h_start To h_end Step h_step
		Print #file, Using " #####.#####"; seans_str_out(t).p(h);
	Next
	Print #file,
Next t
Print "OK"

Close #file

Print
Print "OK"
break


''' =======================================================================



Sub LoadFiles(ByVal Directory As String)

	Dim As ZString Ptr lst  ' список файлов
	Dim As Integer lst_len  ' длина списка файлов
	Dim As Integer seans_num' количество сеансов в директории

	Dim As ZString*256 filename
	Dim As Integer isM

	Dim As Integer i
	Dim As Integer symbol = 0

	Dim As seans2_data seans

	lst = Allocate (2*1024*1024*SizeOf(Byte)) ' выделить на список файлов 2 Мбайта
	If lst = NULL Then
		Print "Ошибка памяти!!!", seans_loaded+seans_num, Directory
		break
	EndIf


	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) 'получить имя файла из списка
		If seans2_test(Directory+"/"+filename) > 0 Then
			seans_num += 1
		EndIf
	Next i

	Print seans_loaded+seans_num,

	ReDim Preserve As seans_struct seans_str(0 To seans_loaded+seans_num-1)


	For i = 0 To lst_len-1

		filelist_get_filename(lst, @filename, i)

		isM = seans2_test(directory + "/" + filename)

		If (isM <> 0) Then
			seans2_load ( directory + "/" + filename, @seans )

			Print #1, filename

			seans_str(seans_loaded).filename = filename
			seans_str(seans_loaded).filename_full = directory + "/" + filename

			seans_str(seans_loaded).time_decimal = time_2decimal(seans.Hour1, seans.Minute1, seans.Second1)
			seans_str(seans_loaded).time_computer = date_2unixtime(seans.Day1, seans.Month1, seans.Year1, seans.Hour1, seans.Minute1, seans.Second1)

			For h As Integer = 0 To 679
				seans_str(seans_loaded).p1(h) = seans.datps1(h) - CDbl(seans.dat03(h))*CDbl(seans.dat03(h))/1463.0
				seans_str(seans_loaded).p2(h) = seans.datps2(h) - CDbl(seans.dat04(h))*CDbl(seans.dat04(h))/1463.0
				seans_str(seans_loaded).m1(h) = 0
				seans_str(seans_loaded).m2(h) = 0
			Next h

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

Sub Vis_array_load()

	Dim As Integer i, j
	Dim As Double d(0 To 1)
	Dim As Double max

	For i = 0 To seans_loaded-1
		vis_array(0, i) = seans_str(i).p1(hCur)
		vis_array(1, i) = seans_str(i).p2(hCur)
	Next i


	d(0) = -1e200
	d(1) = -1e200
	For i = 0 To seans_loaded-1

		If vis_array(0, i) > d(0) And seans_str(i).m1(hCur) = 0 Then
			d(0) = vis_array(0, i)
		EndIf

		If vis_array(1, i) > d(1) And seans_str(i).m2(hCur) = 0 Then
			d(1) = vis_array(1, i)
		EndIf

	Next i


	If d(0) > d(1) Then
		max = d(0)
	Else
		max = d(1)
	EndIf

	For j = 0 To 1
		For i = 0 To seans_loaded-1
			vis_array(j, i) /= max
		Next i
	Next j




	For i = 0 To 679
		vis_array_alt(0, i) = seans_str(position).p1(i)
		vis_array_alt(1, i) = seans_str(position).p2(i)
	Next

	Dim As Double pn1 = 0
	Dim As Double pn2 = 0
	Dim As Integer c1 = 0
	Dim As Integer c2 = 0
	For i = 500 To 600
		If seans_str(position).m1(i) = 0 Then
			pn1 += vis_array_alt(0, i)
			c1 += 1
		EndIf

		If seans_str(position).m1(i) = 0 Then
			pn2 += vis_array_alt(1, i)
			c2 += 1
		EndIf

	Next
	pn1 /= c1
	pn2 /= c2


	For i = 0 To 679
		vis_array_alt(0, i) -= pn1
		vis_array_alt(1, i) -= pn2
	Next


	d(0) = -1e200
	d(1) = -1e200
	For i = 50 To 679

		If vis_array_alt(0, i) > d(0) And seans_str(position).m1(i) = 0 Then
			d(0) = vis_array_alt(0, i)
		EndIf

		If vis_array_alt(1, i) > d(1) And seans_str(position).m2(i) = 0 Then
			d(1) = vis_array_alt(1, i)
		EndIf

	Next i

	If d(0) > d(1) Then
		max = d(0)
	Else
		max = d(1)
	EndIf

	For j = 0 To 1
		For i = 0 To 679
			vis_array_alt(j, i) /= max
		Next i
	Next j

End Sub

''' =======================================================================

Sub Filter(wnd_width As Integer, lev As Double, direction As Integer)

	Dim h As Integer
	Dim t As Integer

	Dim As Integer num, sm
	Dim As Double mean, dev

	If direction = 0 Then

		For h = 0 To 679
			For t = seans_loaded-1-wnd_width To 1 Step -1

				num = 0
				mean = 0
				For sm = 0 To wnd_width-1
					If seans_str(t+sm).m1(h) = 0 Then
						mean += seans_str(t+sm).p1(h)
						num += 1
					EndIf
				Next sm

				If num > 9 Then
					mean /= num

					dev = 0
					For sm = 0 To wnd_width-1
						If seans_str(t+sm).m1(h) = 0 Then
							dev += ( seans_str(t+sm).p1(h) - mean )^2
						EndIf
					Next sm

					dev = Sqr(dev/(num-1))

					If Abs(seans_str(t-1).p1(h)-mean) > lev*dev Then
						seans_str(t-1).m1(h) = 1
					EndIf

				EndIf

			Next t
		Next h

		For h = 0 To 679
			For t = seans_loaded-1-wnd_width To 1 Step -1

				num = 0
				mean = 0
				For sm = 0 To wnd_width-1
					If seans_str(t+sm).m2(h) = 0 Then
						mean += seans_str(t+sm).p2(h)
						num += 1
					EndIf
				Next sm

				If num > 9 Then
					mean /= num

					dev = 0
					For sm = 0 To wnd_width-1
						If seans_str(t+sm).m2(h) = 0 Then
							dev += ( seans_str(t+sm).p2(h) - mean )^2
						EndIf
					Next sm

					dev = Sqr(dev/(num-1))

					If Abs(seans_str(t-1).p2(h)-mean) > lev*dev Then
						seans_str(t-1).m2(h) = 1
					EndIf

				EndIf

			Next t
		Next h

	Else

		For h = 0 To 679
			For t = 0 To seans_loaded-1-wnd_width

				num = 0
				mean = 0
				For sm = 0 To wnd_width-1
					If seans_str(t+sm).m1(h) = 0 Then
						mean += seans_str(t+sm).p1(h)
						num += 1
					EndIf
				Next sm

				If num > 9 Then
					mean /= num

					dev = 0
					For sm = 0 To wnd_width-1
						If seans_str(t+sm).m1(h) = 0 Then
							dev += ( seans_str(t+sm).p1(h) - mean )^2
						EndIf
					Next sm

					dev = Sqr(dev/(num-1))

					If Abs(seans_str(t+wnd_width).p1(h)-mean) > lev*dev Then
						seans_str(t+wnd_width).m1(h) = 1
					EndIf

				EndIf

			Next t
		Next h


		For h = 0 To 679
			For t = 0 To seans_loaded-1-wnd_width

				num = 0
				mean = 0
				For sm = 0 To wnd_width-1
					If seans_str(t+sm).m2(h) = 0 Then
						mean += seans_str(t+sm).p2(h)
						num += 1
					EndIf
				Next sm

				If num > 9 Then
					mean /= num

					dev = 0
					For sm = 0 To wnd_width-1
						If seans_str(t+sm).m2(h) = 0 Then
							dev += ( seans_str(t+sm).p2(h) - mean )^2
						EndIf
					Next sm

					dev = Sqr(dev/(num-1))

					If Abs(seans_str(t+wnd_width).p2(h)-mean) > lev*dev Then
						seans_str(t+wnd_width).m2(h) = 1
					EndIf

				EndIf

			Next t
		Next h

	EndIf

End Sub