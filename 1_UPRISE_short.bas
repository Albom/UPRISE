
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
Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim Shared timestr As ZString*16 ' переменная для представления времени в виде строки

ReDim Preserve Shared As seans_struct seans_str(0 To 1)
Dim Shared seans_loaded As Integer   ' сеансов загружено


Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Integer

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


' Сортировка сеансов по времени
Print "Сортировка по времени... ";
qsort(@seans_str(0), seans_loaded, SizeOf(seans_struct), @seans_struct_time_compare)
Print "OK"




Print "Обработка данных... ";

If (is_clear <> 0) Then

	Dim As Integer num, sm
	Dim As Double mean, dev

	' фильтрация (1-й раз)

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





	' фильтрация (2-й раз)

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


EndIf


For t = 0 To seans_loaded-1
	For h = 0 To 679
		seans_str(t).p(h) = seans_str(t).p1(h) + seans_str(t).p2(h)
	Next h
Next t



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



Print "OK"






file = FreeFile()
Open "Short.txt" For Output As #file

Print "Вывод результатов в файлы... ";

Print #file, "       0";
For h = h_start To h_end Step h_step
	Print #file, Using "        ####"; seans2_altS(h);
Next h
Print #file,

For t = 0 To seans_current-1
	Print #file, Using " ##.####"; seans_str_out(t).time_decimal;
	For h = h_start To h_end Step h_step
		Print #file, Using " ##.####^^^^"; seans_str_out(t).p(h);
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

