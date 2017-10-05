
#Include Once "albom_lib.bi"	'Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		'Подключение лога
#Include "fbgfx.bi"			'Подключение графической библиотеки
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "dir.bi"

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf

Type seans_struct
	Dim filename_full As ZString*256
	Dim filename As ZString*64
	Dim seans As seans1c_data
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
Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim Shared timestr As ZString*16 ' переменная для представления времени в виде строки

ReDim Preserve Shared As seans_struct seans_str(0 To 1)
Dim Shared seans_loaded As Integer   ' сеансов загружено

Dim As Integer VISIBLE = 0 ' видны ли вырезанные участки?
Dim Shared As Integer CUR = 0 ' текущая позиция (текущий сеанс)
Dim Shared As Integer H, HMIN, HMAX ' текущая, минимальная и максимальная высота
Dim As Integer DX = 1 ' масштаб по оси x
Dim As Integer DY = 300 ' масштаб по оси y
Dim As Integer Y0 = 300 ' начальное значение по оси y
Dim As Integer START_X = 0
Dim As Integer POINTS_START = 0
Dim As Integer POINTS_END = 6
ReDim Shared vis_array(0 To 6, 0 To 10) As Double ' буфер для отображения графиков
Dim Shared As Integer MAX_Y = 0
Dim Shared As Integer is_noise = 1 ' вычитать шум? (1 - нет, -1 - да)
Dim Shared As Integer CHANNEL = 1' текущий канал


Dim key As Integer

Declare Sub Vis_array_load()
Declare Sub AutomaticClear()
Declare Sub HelpPrint()
Declare Sub ACFPrint()
Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Long

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String Config_driver = "GDI"

''' =======================================================================

Open Err For Output As #1

' Загрузка конфигурационного файла
Dim As Integer file
file = FreeFile()
Open "config_screen.dat" For Input As #file
If Err <> 0 Then
	PrintErrorToLog(ErrorFilter, __FILE__, __LINE__)
	End
EndIf
Input #file, Config_driver
Close #file

SetEnviron("fbgfx="+Config_driver)

Screen 20
#Include Once "albom_font.bi"
Dim As String driver
ScreenInfo ,,,,,,driver
Print #1, "Screen driver: "; driver

Dim As String  tmp
'Dim Shared As Integer pulseLength
file = FreeFile()
/'
Open "config.dat" For Input As #file
Input #file, tmp
Input #file, tmp
Input #file, tmp
Input #file, tmp
Input #file, pulseLength
Close #file

If (pulseLength <> 663) And (pulseLength <> 795) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf
'/

Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "View - программа просмотра C-файлов системы К1"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Color 8
Print
Print "================================"
Print "Программа собрана " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
Print "================================"
Print

Color 11

Print "Исходные данные, находящиеся в папке " + Chr(34) + "in" + Chr(34) + ":"
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
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

If (d_day < 1) Or (d_month < 1) Or (d_year < 1996) Or (d_ndays < 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf

Print "Загрузка... ";

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
Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"


' Сортировка сеансов по времени
Print "Сортировка по времени... ";
qsort(@seans_str(0), seans_loaded, SizeOf(seans_struct), @seans_struct_time_compare)
Print "OK"

HMIN = 11
HMAX = 350
H = HMIN


ReDim vis_array(0 To 6, 0 To seans_loaded) As Double ' буфер для отображения графиков

Vis_array_load() ' загрузить данные для отображения

Do

	ScreenLock 'начинаем вывод на экран

	Cls()

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


	' рисование карты вырезанных сеансов
	For i = START_X To (seans_loaded-2)
		If seans_str(i).seans.m(H) <> 0 Then
			Line(5+(i-START_X)*DX, 768-24)-(5+(i+1-START_X)*DX, 768-21),  15, bf
		End If
	Next i

	' метка максимального значения, на которое нормируются значения АКФ
	Line(5+(MAX_Y-START_X)*DX, 768-24)-(5+(1+MAX_Y-START_X)*DX, 768-21),  12, bf

	Color 10
	Print seans_str(CUR).filename;

	If seans_str(CUR).isM = 2 Then
		Print  "*",
	Else
		Print " ",
	End If
	Color 14
	Print Using "Время: ###.###  "; seans_str(CUR).time_decimal;
	time_2str(seans_str(CUR).seans.Hour1, seans_str(CUR).seans.Minute1, seans_str(CUR).seans.Second1, @timestr)
	Print timestr,
	Color 11
	Print Using "Высота: № ###"; H+1;
	If H > HMAX-100 Then
		Print  "*";
	Else
		Print " ";
	End If

'	If pulseLength = 663 Then
		Print Using "  #### км"; seans1c_alt(H);
'	Else
'		Print Using "  #### км"; seans1c_alt(H);
'	EndIf

	Color 10
	Print ,CHANNEL;
	Color 12
	Print ,,
	Print "F1: Помощь     Ctrl+Q: Выход"
	Print


	ScreenUnLock 'завершаем вывод на экран

	key = GetKey()

	'	Cls
	'	Print key
	'	Sleep 1000

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

		Case KEY_DOWN
			If START_X < seans_loaded-1 Then START_X += 1 End If

		Case KEY_UP
			If START_X > 0  Then START_X -= 1  End If

		Case KEY_CTRL_DOWN
			If START_X < seans_loaded-10 Then START_X += 10 End If

		Case KEY_CTRL_UP
			If START_X > 10  Then START_X -= 10  End If


		Case KEY_H
			Color 15
			Input "H: ", H
			H = H-1
			If H < HMIN Then H = HMIN End If
			If H > HMAX-1 Then H = HMAX-1 End If
			Vis_array_load ' загрузить данные для отображения

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
			Vis_array_load ' загрузить данные для отображения

		Case KEY_PAGE_DOWN
			If H > HMIN Then H = H - 1 End If
			Vis_array_load ' загрузить данные для отображения

		Case KEY_SPACE
			If seans_str(CUR).seans.m(H) = 0 Then
				seans_str(CUR).seans.m(H) = 1
			Else
				seans_str(CUR).seans.m(H) = 0
			End If
			seans_str(CUR).isM = 2
			seans1c_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			Vis_array_load ' загрузить данные для отображения

		Case KEY_V
			If VISIBLE = 0 Then
				VISIBLE = 1
			Else
				VISIBLE = 0
			End If
			Vis_array_load ' загрузить данные для отображения

		Case KEY_F1
			HelpPrint()
			Vis_array_load ' загрузить данные для отображения

		Case KEY_R
			ACFPrint()
			Vis_array_load ' загрузить данные для отображения

		Case KEY_HOME
			CUR=START_X

		Case KEY_END
			If START_X+(1024\DX)-1 < seans_loaded Then
				CUR = START_X+(1024\DX)-1
			Else
				CUR=seans_loaded-1
			End If

		Case KEY_A
			Color 15
			AutomaticClear()
			Vis_array_load ' загрузить данные для отображения

		Case KEY_TAB
			If CUR + 10 < seans_loaded-1 Then CUR = CUR + 10

		Case KEY_N
			is_noise = -is_noise
			Vis_array_load ' загрузить данные для отображения

		Case KEY_DEL
			seans_str(CUR).seans.m(H) = 1
			seans_str(CUR).isM = 2
			seans1c_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			If CUR < seans_loaded-1 Then CUR = CUR + 1 End If
			Vis_array_load ' загрузить данные для отображения


		Case KEY_CTRL_DEL
			For i = 0 To 359
				seans_str(CUR).seans.m(i) = 1
			Next i
			seans_str(CUR).isM = 2
			seans1c_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			Vis_array_load


		Case KEY_BACKSPACE
			seans_str(CUR).seans.m(H) = 1
			seans_str(CUR).isM = 2
			seans1c_saveM3(seans_str(CUR).filename_full, @(seans_str(CUR).seans))
			If CUR > 0 Then CUR = CUR - 1  End If
			Vis_array_load ' загрузить данные для отображения

		Case KEY_1
			CHANNEL = 1
			Vis_array_load ' загрузить данные для отображения

		Case KEY_2
			CHANNEL = 2
			Vis_array_load ' загрузить данные для отображения

		Case KEY_3
			CHANNEL = 3
			Vis_array_load ' загрузить данные для отображения

		Case KEY_4
			CHANNEL = 4
			Vis_array_load ' загрузить данные для отображения

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
	Print "View - программа просмотра C-файлов системы К1"
	Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
	Color 8
	Print
	Print "================================"
	Print "Программа собрана " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
	Print "================================"
	Color 15
	Print
	Print "Управление:"
	Print " Alt + Enter      Переключиться из оконного режима в полноэкранный и наоборот (работает не на всех системах)"
	Print "       Left Right Перемещение по сеансам"
	Print "       Home       Перемещение курсора на сеанс в начале экрана"
	Print "       End        Перемещение курсора на сеанс в конце экрана"
	Print "       Tab        Перемещение курсора на 10 сеансов вправо"
	Print "       Up Down    Сдвиг сеансов по оси времени на 1"
	Print "Ctrl + Up Down    Сдвиг сеансов по оси времени на 10"
	Print "        +   -     Изменение масштаба по оси времени"
	Print "       PgUp PgDn  Перемещение по высотам"
	Print "       H          Задать текущую высоту"
	Print "       Пробел     Вырезать данные на текущей высоте из текущего сеанса"
	Print "       Del        Вырезать данные на текущей высоте из текущего сеанса и сместить курсор на следующий сеанс"
	Print "       BackSpace  Вырезать данные на текущей высоте из текущего сеанса и сместить курсор на предыдущий сеанс"
	Print "       V          Отображать или скрывать вырезанные данные"
	Print "       O          Выбор ординат для отображения"
	Print "       R          Показать коэффициент корреляции сигнала НР"
	Print "       N          Выбор режима - сигнал+шум или сигнал (коэффициент корреляции)"
	Print "       1 2 3 4    Выбор канала"
	Print "       A          Автоматическое удаление когерентных отражений"
	Print "       F1         Вызов этой помощи"
	Print "Ctrl + P          Сохранение экрана в файл screen.bmp"
	Print "Ctrl + Q          Выход из программы"
	Print
	Color 12
	Print "Нажмите Enter"

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
	Dim As Integer NUM = 6

	ReDim As Double max_a(0 To seans_loaded-1) ' массив для поиска max (с учётом меток)
	'	ReDim As Double noise(0 To seans_loaded-1, 0 To 6) ' массив АКФ шума

	/'
	If is_noise < 0 Then
		For i = 0 To seans_loaded-1
			seans1c_noise(@seans_str(i).seans, @noise(i, 0), 19, 500, 600)
		Next i
	Else
		For i = 0 To seans_loaded-1
			For j = 0 To 6
				noise(i, j) = 0
			Next
		Next
	EndIf
'/

	For j = 0 To NUM ' Загружаем ординаты для отображения
		For i = 0 To seans_loaded-1
			vis_array(j, i) = seans_str(i).seans.dat1(H, j) '- noise(i, j)
		Next i
	Next j

	d = -1e200
	For i = 0 To seans_loaded-1
		If vis_array(0, i) > d And seans_str(i).seans.m(H) = 0 Then
			d = vis_array(0, i)
			MAX_Y = i
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

		Case 0
			Exit Sub

		Case 1
			For t = 0 To seans_loaded-1 ' по времени
				seans_str(t).isM = 1
				seans1c_saveM0(seans_str(t).filename_full)
				symbol = Print_process(symbol)
				For h1 As Integer = 0 To 359
					seans_str(t).seans.m(h1) = 0
				Next h1
			Next t

		Case 2 ' проверка предыдущей точки
			Input "Ширина окна: ", wnd_width
			Input "Уровень: ", lev

			For ord = 0 To 6 ' по ординатам
				For h1 As Integer = 0 To 359
					For t = seans_loaded-1-wnd_width To 1 Step -1 ' по времени

						num = 0
						mean = 0
						For sm = 0 To wnd_width-1
							If seans_str(t+sm).seans.m(h1) = 0 Then
								mean += seans_str(t+sm).seans.dat1(h1, ord)
								num += 1
							EndIf
						Next sm

						If num > 9 Then
							mean /= num

							dev = 0
							For sm = 0 To wnd_width-1
								If seans_str(t+sm).seans.m(h1) = 0 Then
									dev += ( seans_str(t+sm).seans.dat1(h1, ord) - mean )^2
								EndIf
							Next sm

							dev = Sqr(dev/(num-1))

							If Abs(seans_str(t-1).seans.dat1(h1, ord)-mean) > lev*dev Then
								seans_str(t-1).seans.m(h1) = 1
							EndIf

						EndIf

					Next t
				Next h1
			Next ord

			For t = 0 To seans_loaded-1
				seans_str(t).isM = 2
				seans1c_saveM3(seans_str(t).filename_full, @(seans_str(t).seans))
				symbol = Print_process(symbol)
			Next t

		Case 3 ' проверка следующей точки
			Input "Ширина окна: ", wnd_width
			Input "Уровень: ", lev

			For ord = 0 To 6 ' по ординатам
				For h1 As Integer = 0 To 359
					For t = 0 To seans_loaded-1-wnd_width ' по времени

						num = 0
						mean = 0
						For sm = 0 To wnd_width-1
							If seans_str(t+sm).seans.m(h1) = 0 Then
								mean += seans_str(t+sm).seans.dat1(h1, ord)
								num += 1
							EndIf
						Next sm

						If num > 9 Then
							mean /= num

							dev = 0
							For sm = 0 To wnd_width-1
								If seans_str(t+sm).seans.m(h1) = 0 Then
									dev += ( seans_str(t+sm).seans.dat1(h1, ord) - mean )^2
								EndIf
							Next sm

							dev = Sqr(dev/(num-1))

							If Abs(seans_str(t+wnd_width).seans.dat1(h1, ord)-mean) > lev*dev Then
								seans_str(t+wnd_width).seans.m(h1) = 1
							EndIf

						EndIf

					Next t
				Next h1
			Next ord

			For t = 0 To seans_loaded-1
				seans_str(t).isM = 2
				seans1c_saveM3(seans_str(t).filename_full, @(seans_str(t).seans))
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
	Dim As Double r(0 To 18)'АКФ сигнала НР
	Dim As Double n(0 To 18)'АКФ шума
	Dim As Double sn
	Dim As Integer is_noise_ACF = 1


	Do

		For i = 0 To 18
			n(i) = 0
		Next i

		k = 0
		For j = 140 To 180
			If seans_str(CUR).seans.m(j) = 0 Then
				For i = 0 To 6
					n(i) += seans_str(CUR).seans.dat1(j, i)
				Next i
				k += 1
			EndIf
		Next j

		array_norm_d(@n(0), @n(0), k, 7) ' нормировать на количество хороших точек k в шумовом участке

		If is_noise_ACF > 0 Then ' вычитать шум или нет?
			For i = 0 To 6
				r(i) = seans_str(CUR).seans.dat1(H, i) - n(i)
			Next i
		Else
			For i = 0 To 6
				r(i) = seans_str(CUR).seans.dat1(H, i)
			Next i
		EndIf

		If n(0) <> 0 Then
			sn = r(0)/n(0)
		Else
			sn = 0
		EndIf

		array_norm0_d(@r(0), @r(0), 7)
		array_norm0_d(@n(0), @n(0), 7)




		' Рисование

		Cls

		Color 11
		If is_noise_ACF > 0 Then  ' вычитать шум или нет?
			Print "Режим: с  ";
			Color 13
			Print Using "с/ш = ###.##   "; sn;
		Else
			Print "Режим: с+ш               ";
		EndIf

		Color 15
		Print Using "Время: ###.###    "; seans_str(CUR).time_decimal;
		time_2str(seans_str(CUR).seans.Hour1, seans_str(CUR).seans.Minute1, seans_str(CUR).seans.Second1, @timestr)
		Print timestr,

		Color 11
		Print Using "Высота: № ###"; H+1;
		Print Using "  #### км  "; seans1c_alt(H);

		Color 7
		Print
		Print "Перемещение по времени: Влево/Вправо     Перемещение по точке АКФ: Вверх/Вниз     Перемещение по высоте: PageDown/PageUp"
		Print "С / С+Ш: N"                               
		Color 12
		Print
		Print "Для выхода нажмите Enter"

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

		For i = 0 To 6
			Line(x0+i*dxdy, 400-dxdy*5)-(x0+i*dxdy, 400+dxdy*2), 7, , &b0000000011110000
		Next i

		For i = 0 To 5
			Line (x0+i*dxdy, 400-5*dxdy*r(i))-(x0+(i+1)*dxdy, 400-5*dxdy*r(i+1)), 14
			Line (x0+i*dxdy, 400-5*dxdy*n(i))-(x0+(i+1)*dxdy, 400-5*dxdy*n(i+1)), 9
		Next i

		For i = 0 To 6
			Circle (x0+i*dxdy, 400-5*dxdy*r(i)), 3, 10
			Circle (x0+i*dxdy, 400-5*dxdy*n(i)), 3, 9
		Next i

		For i = 1 To 3
			Draw String (x0+i*dxdy*2+2-12, 400+2*dxdy+2), Str(CInt((i+3)*39.285)*2) /'Left(Str(i*30.555*2), 5)'/, 15
		Next i

		For i = 0 To 4 ' выводим надписи от 1 до 0.2
			Draw String (x0-32, 400-5*dxdy+i*dxdy), Left(Str( (100-20*i)/100 ), 5), 15
		Next i

		' выводим надпись 0
		Draw String (x0-40, 400-5*dxdy+i*dxdy), "   0", 15

		For i = 6 To 7 ' выводим надписи от -0.2 до -0.4
			Draw String (x0-40, 400-5*dxdy+i*dxdy), Left(Str( (100-20*i)/100 ), 5), 15
		Next i



		key = GetKey

		Select Case key


			Case KEY_RIGHT
				If CUR < seans_loaded-1 Then CUR = CUR + 1 End If

			Case KEY_LEFT
				If CUR > 0 Then CUR = CUR - 1  End If

			Case KEY_N
				is_noise_ACF *= -1

			Case KEY_PAGE_UP
				If H < HMAX-1 Then H += 1 End If
				Vis_array_load() ' загрузить данные для отображения

			Case KEY_PAGE_DOWN
				If H > HMIN Then H -= 1 End If
				Vis_array_load() ' загрузить данные для отображения

		End Select


	Loop Until key = KEY_ENTER

	Color 15, 0
	Cls



End Sub



''' =======================================================================



Sub LoadFiles(ByVal Directory As String)

	Dim As ZString Ptr lst  ' список файлов
	Dim As Integer lst_len  ' длина списка файлов
	Dim As Integer seans_num' количество сеансов в директории

	Dim As ZString*256 filename
	Dim As Integer isM

	Dim As Integer i
	Dim As Integer symbol = 0

	lst = Allocate (2*1024*1024*SizeOf(Byte)) ' выделить на список файлов 2 Мбайта
	If lst = NULL Then
		Print "Ошибка памяти!!!", seans_loaded+seans_num, Directory
		break
	EndIf


	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) 'получить имя файла из списка
		If seans1c_test(Directory+"/"+filename) > 0 Then
			seans_num += 1
		EndIf
	Next i

	Print seans_loaded+seans_num,

	ReDim Preserve As seans_struct seans_str(0 To seans_loaded+seans_num-1)

	/'
	If Err = 4 Then
		Print "Ошибка памяти!!!", seans_loaded+seans_num, Directory
		break
	EndIf
'/

	For i = 0 To lst_len-1

		filelist_get_filename(lst, @filename, i)

		isM = seans1c_test(directory + "/" + filename)

		If (isM <> 0) Then

			seans1c_load ( directory + "/" + filename, @(seans_str(seans_loaded).seans) )

/'
Open "1.csv" For Output As #4

For hx As Integer = 0 To 359
	For tau As Integer = 0 To 6
		Print #4, seans_str(seans_loaded).seans.dat1(hx, tau); "; ";
	Next tau
	Print #4,
Next hx

Close #4
Print "OK"
break
'/
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


Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Long
	Return ( CPtr(seans_struct Ptr, elem1) -> time_computer ) - ( CPtr(seans_struct Ptr, elem2) -> time_computer )
End Function


''' =======================================================================

