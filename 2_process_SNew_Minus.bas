
#Include Once "albom_lib.bi"	' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"
#Include "dir.bi"

'''==============================================

Type seans_struct_in
	Dim seans_header	As seans_header Ptr
	Dim time_computer	As Integer
	Dim filename As ZString*256
End Type



Type seans_struct_out
	Dim datCos(0 To 18)		As Double  Ptr
	Dim datSin(0 To 18)		As Double  Ptr
	Dim datCosTrap(0 To 18)	As Double  Ptr
	Dim datSinTrap(0 To 18)	As Double  Ptr
	Dim m							As Integer Ptr
	Dim pShort					As Double  Ptr
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

Dim Shared As ZString Ptr lst  ' список файлов

Dim Shared As seans_struct_in Ptr seans_str_in 'массив структур данных из сеансов
Dim Shared As Integer seans_loaded   ' сеансов загружено

Dim Shared As Integer seans_out_num   ' количество выходных сеансов

Dim Shared As seans_struct_out seans_str_out (0 To 679) 'массив структур данных из сеансов
Dim Shared As seans_struct_time seans_str_time 'структур данных для времени (которое хранится отдельно от seans_str_out для экономии места)

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
Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim As Integer Day1, Month1, Year1, Hour1, Minute1, Second1

Dim As Double Ptr trand
Dim As Integer tNak
Dim As Integer tStep
Dim As Integer n2

Dim Shared As Double razr(0 To 350)
Dim As String razr_filename

Dim Shared As Double R0DAC_1(0 To 18) ' 0 АЦП 1-го косинусного канала
Dim Shared As Double R0DAC_2(0 To 18) ' 0 АЦП 1-го синусного канала
Dim Shared As Double R0DAC_3(0 To 18) ' 0 АЦП 2-го косинусного канала
Dim Shared As Double R0DAC_4(0 To 18) ' 0 АЦП 2-го синусного канала

Dim As as_file_struct as_file_str ' буфер для выходного файла

Dim Shared noiseAcfCos(0 To 18)	As Double  Ptr
Dim Shared noiseAcfSin(0 To 18)	As Double  Ptr
Dim Shared noisePShort As Double  Ptr
'Dim Shared signalToNoiseRatio(0 To 679) As Double  Ptr
Dim Shared signalToNoiseRatioShort(0 To 679) As Double  Ptr

Dim As Integer partrap = 0

Dim As Integer is_divide=1

'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"


file = FreeFile()
Open "config/config.dat" For Input As #file
Input #file, razr_filename
Close #file

If razr_load(razr_filename, @razr(0), 330) = 0 Then ' загрузка разрядника
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf

Open Err For Output As #1


Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"

Print
Color 12
Print "Без вычитания шума, учёта разрядника и трапецеидального суммирования!"

Print
Color 7
Print "Processing - программа подготовки данных (S-файлов системы К3) к решению обратной задачи"
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

Print
Input "Введите время накопления (в мин): ", Tnak
Input "Введите шаг перемещения окна (в мин): ", Tstep
Input "Введите количество точек для интерполяции: ", n2
'Print
'Input "Введите параметр трапецеидального суммирования: ", partrap
partrap = 0

If (Tnak < 1) Or (Tstep < 1) Or (n2 < 1) Or (partrap < 0) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf



' загрузка сеансов
Print
Print "Сканирование директорий... ";

seans_loaded = 0
seans_str_in = NULL

lst = Allocate (2*1024*1024*SizeOf(Byte)) ' выделить на список файлов 2 Мбайта
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

If seans_loaded = 0 Then  ' если ни один сеанс не загружен, выходим, предварительно выдав ошибку в лог
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"

Print #1, Str(seans_loaded)+" files found"

MkDir(SEANS_DIR_OUT)

DirectoryOutput += directory
MkDir(SEANS_DIR_OUT +DirectoryOutput)
MkDir(SEANS_DIR_OUT +DirectoryOutput+"/step2")

file = FreeFile()
Open SEANS_DIR_OUT +DirectoryOutput+"/step2/input.txt" For Output As #file
Print #file, "Время накопления (в мин): "; Tnak
Print #file, "Шаг перемещения окна (в мин): "; Tstep
Print #file, "Количество точек для интерполяции: "; n2
Print #file, "Параметр трапецеидального суммирования: "; partrap
Close #file


' сортировка сеансов по времени
Print "Сортировка по времени... ";
qsort(seans_str_in, seans_loaded, SizeOf(seans_struct_in), @seans_struct_time_compare)
Print "OK"

Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"
Print "Загрузка данных... ";


' расчёт количества выходных файлов
seans_out_num = 1
For t = 0 To seans_loaded-1-1 ' по времени

	If ( seans_str_in[t+1].time_computer - seans_str_in[t].time_computer ) > 119 Then
		seans_out_num += (seans_str_in[t+1].time_computer - seans_str_in[t].time_computer)/60
	Else
		seans_out_num += 1
	EndIf

Next t


'выделяем память на выходные сеансы

For h = 0 To 679 ' по высоте

	For tau = 0 To 18

		seans_str_out(h).datCos(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datCos(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

		seans_str_out(h).datSin(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datSin(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

		seans_str_out(h).datCosTrap(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datCosTrap(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

		seans_str_out(h).datSinTrap(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datSinTrap(tau) = NULL Then
			PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
			End
		EndIf

	Next tau

	seans_str_out(h).m = Callocate( seans_out_num, SizeOf(Integer) )
	If seans_str_out(h).m = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	seans_str_out(h).pShort = Callocate( seans_out_num, SizeOf(Double) )
	If seans_str_out(h).pShort = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

Next h




'выделяем память на время
seans_str_time.time_computer = Callocate( seans_out_num, SizeOf(Integer) )
If seans_str_time.time_computer = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

'выделяем память на массивы строк для даты и времени
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

For i = 0 To seans_out_num-1 'выделяем память на строки для даты и времени
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



' формирование АКФ сигнала НР + шума

' разбрасываем имеющиеся сеансы в выходные массивы
i = 0 ' позиция в выходном массиве
For t = 0 To seans_loaded-1-1 ' по времени

	Print_process_percent((t*100)/seans_loaded)

	date_2str(  seans_str_in[t].seans_header->day1,_
	seans_str_in[t].seans_header->month1, _
	seans_str_in[t].seans_header->year1, _
	seans_str_time.date_[i] )

	time_2str(  seans_str_in[t].seans_header->hour1, _
	seans_str_in[t].seans_header->minute1, _
	seans_str_in[t].seans_header->second1, _
	seans_str_time.time_[i] )

	seans_str_time.time_computer[i] = seans_str_in[t].time_computer

	Dim As seans2_data seans
	seans2_load(seans_str_in[t].filename, @seans)


	For h = 0 To 679 ' по высотам
		If seans.m(h) = 0 Then
			seans_str_out(h).m[i] = 1

			If h < 679-19 Then
				For tau = 0 To 18
					R0DAC_1(tau) = CDbl(seans.dat01(h))*CDbl(seans.dat01(h+tau))/1463.0
					R0DAC_3(tau) = CDbl(seans.dat02(h))*CDbl(seans.dat02(h+tau))/1463.0
					R0DAC_2(tau) = CDbl(seans.dat01(h))*CDbl(seans.dat02(h+tau))/1463.0
					R0DAC_4(tau) = CDbl(seans.dat02(h))*CDbl(seans.dat01(h+tau))/1463.0
				Next tau
			Else
				For tau = 0 To 18
					R0DAC_1(tau) = 0
					R0DAC_3(tau) = 0
					R0DAC_2(tau) = 0
					R0DAC_4(tau) = 0
				Next tau
			EndIf

			For tau = 0 To 18 ' по задержке
				seans_str_out(h).datCos(tau)[i] = ( CDbl(seans.dat1(h, tau)) - R0DAC_1(tau) ) + ( CDbl(seans.dat3(h, tau)) - R0DAC_3(tau) )
				seans_str_out(h).datSin(tau)[i] = ( CDbl(seans.dat4(h, tau)) - R0DAC_4(tau) ) - ( CDbl(seans.dat2(h, tau)) - R0DAC_2(tau) )
			Next tau

			' мощность по короткому импульсу
			seans_str_out(h).pShort[i] = CDbl(seans.Datps1(h)) + CDbl(seans.Datps2(h)) - CDbl(seans.Dat03(h))*CDbl(seans.Dat03(h))/1463.0 - CDbl(seans.Dat04(h))*CDbl(seans.Dat04(h))/1463.0

		EndIf
	Next h

	If ( seans_str_in[t+1].time_computer - seans_str_in[t].time_computer ) > 119 Then
		i += (seans_str_in[t+1].time_computer - seans_str_in[t].time_computer)/60
	Else
		i += 1
	EndIf

Next t

Print_process_percent(1000)

'освобождение памяти, занятой входными сеансами

For t = 0 To seans_loaded-1
	DeAllocate(seans_str_in[t].seans_header)
Next t

DeAllocate(seans_str_in)





' заполнение времени в пропущенные сеансы
For t = 0 To seans_out_num-1 ' по времени
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




Print "Интерполяция... ";


For h = 0 To 679 ' по всем высотам

	Print_process_percent((h*100)/680)

	' Для короткого импульса

	For t = 0 To seans_out_num-1 ' по времени

		' если есть пропуск
		If seans_str_out(h).m[t] <> 1 Then

			Dim As Integer c = 0
			Dim As Double d = 0

			' влево
			i = 0
			Do While (c < n2\2)

				If t-i < 0 Then
					Exit Do
				EndIf

				If seans_str_out(h).m[t-i] = 1 Then
					c += 1
					d += seans_str_out(h).pShort[t-i]
				EndIf

				i += 1

			Loop

			' вправо
			i = 0
			Do While (c < n2\2)

				If t+i > seans_out_num-1 Then
					Exit Do
				EndIf

				If seans_str_out(h).m[t+i] = 1 Then
					c += 1
					d += seans_str_out(h).pShort[t+i]
				EndIf

				i += 1

			Loop

			If c <> 0 Then
				seans_str_out(h).pShort[t] = d/c
			Else
				seans_str_out(h).pShort[t] = 0
			EndIf

		EndIf

	Next t


	For tau = 0 To 18 ' по всем задержкам


		' 1) Для косинусного канала

		For t = 0 To seans_out_num-1 ' по времени

			' если есть пропуск
			If seans_str_out(h).m[t] <> 1 Then

				Dim As Integer c = 0
				Dim As Double d = 0

				' влево
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

				' вправо
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



		' 2) Для синусного канала

		For t = 0 To seans_out_num-1 ' по времени

			' если есть пропуск
			If seans_str_out(h).m[t] <> 1 Then

				Dim As Integer c = 0
				Dim As Double d = 0

				' влево
				i = 0
				Do While (c < n2\2)

					If t-i < 0 Then
						Exit Do
					EndIf

					If seans_str_out(h).m[t-i] = 1 Then
						c += 1
						d += seans_str_out(h).datSin(tau)[t-i]
					EndIf

					i += 1

				Loop

				' вправо
				i = 0
				Do While (c < n2\2)

					If t+i > seans_out_num-1 Then
						Exit Do
					EndIf

					If seans_str_out(h).m[t+i] = 1 Then
						c += 1
						d += seans_str_out(h).datSin(tau)[t+i]
					EndIf

					i += 1

				Loop

				If c <> 0 Then
					seans_str_out(h).datSin(tau)[t] = d/c
				Else
					seans_str_out(h).datSin(tau)[t] = 0
				EndIf

			EndIf

		Next t



	Next tau

Next h

Print_process_percent(1000)
Print "OK"


Print "Вычитание шума... ";


'выделяем память на АКФ шума для каждого сеанса
For tau = 0 To 18
	noiseAcfCos(tau) = Callocate( seans_out_num, SizeOf(Double) )
	If noiseAcfCos(tau) = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
Next tau

For tau = 0 To 18
	noiseAcfSin(tau) = Callocate( seans_out_num, SizeOf(Double) )
	If noiseAcfSin(tau) = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
Next tau

noisePShort = Callocate( seans_out_num, SizeOf(Double) )
If noisePShort = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

For h = 0 To 679
	signalToNoiseRatioShort(h) = Callocate( seans_out_num, SizeOf(Double) )
	If signalToNoiseRatioShort(h) = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
Next h


For t = 0 To seans_out_num-1

	' определяем АКФ шума
	For tau = 0 To 18 ' по задержке
		noiseAcfCos(tau)[t] = 0
		noiseAcfSin(tau)[t] = 0
		For h = 500 To 599 ' по высоте
			noiseAcfCos(tau)[t] += seans_str_out(h).datCos(tau)[t]
			noiseAcfSin(tau)[t] += seans_str_out(h).datSin(tau)[t]
		Next h
		noiseAcfCos(tau)[t] /= 100
		noiseAcfSin(tau)[t] /= 100
	Next tau


	noisePShort[t] = 0
	For h = 500 To 599
		noisePShort[t] += seans_str_out(h).pShort[t]
	Next h
	noisePShort[t] /= 100

	For h = 0 To 679
		seans_str_out(h).pShort[t] -= noisePShort[t]
	Next h


Next t

Print "OK"



Print "Учёт характеристики разрядника... ";

For t = 0 To seans_out_num-1

	' учёт разрядника для профиля мощности по короткому импульсу
	For h = 0 To 679-12 ' по высоте

		Dim As Double r ' значение коэффициента передачи

		If h < 300-12 Then
			r = razr(h+12)
		Else
			r = 1.0
		EndIf

		If r > 1e-6 Then
			seans_str_out(h).pShort[t] /= r
		Else
			seans_str_out(h).pShort[t] = 0
		EndIf

	Next h

Next t



Print "OK"





Print "Трапецеидальное суммирование... ";


For t = 0 To seans_out_num-1

	Print_process_percent((t*100)/seans_out_num)

	' 1) для косинусной составляющей

	For h = 0 To 679

		For tau = 0 To 18
			seans_str_out(h).datCosTrap(tau)[t] = seans_str_out(h).datCos(tau)[t] '!!!
		Next tau
	Next h

	' 2) для синусной составляющей

	For h = 0 To 679

		For tau = 0 To 18
			seans_str_out(h).datSinTrap(tau)[t] = seans_str_out(h).datSin(tau)[t] '!!!
		Next tau

	Next h

Next t

Print_process_percent(1000)
Print "OK"




Print "Накопление по времени... ";



as_file_str.acf = Callocate( 680, SizeOf(acf_struct) )   ' резервируем память для АКФ
If as_file_str.acf = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf


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



	' размещаем данные по высотам
	For h = 0 To 679 ' по высоте
		as_file_str.acf[h].n = h
		as_file_str.acf[h].h = seans2_altL(h)

		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].rc(tau) = 0
			as_file_str.acf[h].rs(tau) = 0
			For i = 0 To tNak-1
				as_file_str.acf[h].rc(tau) += seans_str_out(h).datCosTrap(tau)[t+i]
				as_file_str.acf[h].rs(tau) += seans_str_out(h).datSinTrap(tau)[t+i]
			Next i
			as_file_str.acf[h].rc(tau) /= tNak
			as_file_str.acf[h].rs(tau) /= tNak
		Next tau

	Next h



	' усредняем АКФ шума
	For tau = 0 To 18 ' по задержке
		as_file_str.rnc(tau) = 0
		as_file_str.rns(tau) = 0
		For i = 0 To tNak-1
			as_file_str.rnc(tau) += noiseAcfCos(tau)[t+i]
			as_file_str.rns(tau) += noiseAcfSin(tau)[t+i]
		Next i
		as_file_str.rnc(tau) /= tNak
		as_file_str.rns(tau) /= tNak
	Next tau



	' отношение сигнал/шум
	For h = 0 To 679 ' по высоте
		If as_file_str.rnc(0) <> 0 Then
			as_file_str.acf[h].q = as_file_str.acf[h].rc(0)/as_file_str.rnc(0)-1
		Else
			as_file_str.acf[h].q = 0
		EndIf
	Next h


	For h = 12 To 679 ' по высоте
		as_file_str.acf[h-12].pShort = 0
		For i = 0 To tNak-1
			as_file_str.acf[h-12].pShort += seans_str_out(h).pShort[t+i]
		Next i
		as_file_str.acf[h-12].pShort /= tNak
	Next h

	Dim As Double pShortMean = 0
	For i = 0 To tNak-1
		pShortMean += noisePShort[t+i]
	Next i
	pShortMean /= tNak

	For h = 0 To 679 ' по высоте
		If pShortMean <> 0 Then
			as_file_str.acf[h].qShort = as_file_str.acf[h].pShort/pShortMean
		Else
			as_file_str.acf[h].qShort = 0
		EndIf
	Next h



	' расчёт дисперсии точек АКФ

	For h = 0 To 679
		For tau = 0 To 18 ' по задержке

			as_file_str.acf[h].var(tau) = 0
			For i = 0 To tNak-1
				as_file_str.acf[h].var(tau) += (as_file_str.acf[h].rc(tau) - seans_str_out(h).datCosTrap(tau)[t+i])^2
			Next i
			as_file_str.acf[h].var(tau) /= tNak-1

		Next tau
	Next h


	as_file_save( SEANS_DIR_OUT + DirectoryOutput + "/step2/"+ "AS" + DirectoryOutput + "." + EXT,  @as_file_str) ' запись в файл





	t += tStep
	seans_current += 1

Loop

DeAllocate(as_file_str.acf)   ' освобождаем память с АКФ


Print_process_percent(1000)
Print "OK"





Print
Print "OK"

break





'''==============================================

Sub LoadFiles(ByVal Directory As String)

	Dim As Integer lst_len  ' длина списка файлов
	Dim As Integer seans_num' количество сеансов в директории

	Dim As ZString*256 filename
	Dim As Integer is1s

	Dim As Integer i
	Dim As Integer symbol = 0


	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) 'получить имя файла из списка
		If seans2_test(Directory+"/"+filename) > 0 Then
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

		is1s = seans2_test(directory + "/" + filename)

		If (is1s <> 0) Then

			seans_str_in[seans_loaded].seans_header = Allocate(SizeOf(seans_header))
			If seans_str_in[seans_loaded].seans_header = 0 Then
				PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
				End
			EndIf

			seans_str_in[seans_loaded].filename = directory + "/" + filename

			seans2_load_header ( directory + "/" + filename, seans_str_in[seans_loaded].seans_header )

			seans_str_in[seans_loaded].time_computer = date_2unixtime(_
			seans_str_in[seans_loaded].seans_header->Day1, _
			seans_str_in[seans_loaded].seans_header->Month1, _
			seans_str_in[seans_loaded].seans_header->Year1, _
			seans_str_in[seans_loaded].seans_header->Hour1, _
			seans_str_in[seans_loaded].seans_header->Minute1, _
			seans_str_in[seans_loaded].seans_header->Second1)

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

