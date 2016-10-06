
#Include Once "albom_lib.bi"	' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"

'''==============================================

Type seans_struct_in
	Dim seans 			As seans1s_data
	Dim time_computer	As Integer
End Type



Type seans_struct_out
	Dim datCos(0 To 18)	As Double  Ptr
	Dim m						As Integer Ptr
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

Dim Shared As seans_struct_out seans_str_out(0 To 679) 'массив структур данных из сеансов
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
Dim As Integer isVar
Dim As Integer isR

Dim Shared As Double razr(0 To 350)

Dim Shared As Double R0DAC_1(0 To 18) ' 0 АЦП 1-го косинусного канала
'Dim Shared As Double R0DAC_2(0 To 18) ' 0 АЦП 1-го синусного канала
'Dim Shared As Double R0DAC_3(0 To 18) ' 0 АЦП 2-го косинусного канала
'Dim Shared As Double R0DAC_4(0 To 18) ' 0 АЦП 2-го синусного канала

Dim As as_file_struct as_file_str ' буфер для выходного файла

'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1


Dim As String  tmp
Dim Shared As Integer pulseLength
file = FreeFile()

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



Cls
Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Integrate - программа накопления по времени S-файлов системы К1"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays
Print
Input "Введите время накопления (в мин): ", Tnak
Input "Введите шаг перемещения окна (в мин): ", Tstep
Input "Введите количество точек для интерполяции: ", n2
Input "Расчитывать дисперсию? (0 - нет, 1 - да): ", isVar



If isVar <> 0 Then
	isVar = 1
EndIf


' загрузка сеансов
Print
Print "Загрузка сеансов... ";

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

Print #1, Str(seans_loaded)+" files loaded"


MkDir(SEANS_DIR_OUT)

DirectoryOutput += directory
MkDir(SEANS_DIR_OUT +DirectoryOutput)
MkDir(SEANS_DIR_OUT +DirectoryOutput+"/step1")

file = FreeFile()
Open SEANS_DIR_OUT +DirectoryOutput+"/step1/input.txt" For Output As #file
Print #file, "Время накопления (в мин): "; Tnak
Print #file, "Шаг перемещения окна (в мин): "; Tstep
Print #file, "Количество точек для интерполяции: "; n2
Print #file, "Расчитывать дисперсию? (0 - нет, 1 - да): "; isVar
Close #file


' сортировка сеансов по времени
Print "Сортировка по времени... ";
qsort(seans_str_in, seans_loaded, SizeOf(seans_struct_in), @seans_struct_time_compare)
Print "OK"

Print "Подготовка данных... ";




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

	Next tau

	seans_str_out(h).m = Callocate( seans_out_num, SizeOf(Integer) )
	If seans_str_out(h).m = NULL Then
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

	date_2str(  seans_str_in[t].seans.day1,_
	seans_str_in[t].seans.month1, _
	seans_str_in[t].seans.year1, _
	seans_str_time.date_[i] )

	time_2str(  seans_str_in[t].seans.hour1, _
	seans_str_in[t].seans.minute1, _
	seans_str_in[t].seans.second1, _
	seans_str_time.time_[i] )

	seans_str_time.time_computer[i] = seans_str_in[t].time_computer

	For h = 0 To 679 ' по высотам
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

			seans_str_out(h).datCos(0)[i] = CDbl(seans_str_in[t].seans.datp(h)) - R0DAC_1(tau)

			If h Mod 4 = 0 Then

				For tau = 1 To 18 ' по задержке
					seans_str_out(h).datCos(tau)[i] = CDbl(seans_str_in[t].seans.dat(h\4, tau-1)) - R0DAC_1(tau)
				Next tau

			EndIf

		EndIf

	Next h


	For h = 4 To 679-4 ' по высотам

		If h Mod 4 <> 0 Then

			Dim As Integer h0, h1

			h0 = (h \ 4)*4
			h1 = h0 + 4

			For tau = 1 To 18 ' по задержке
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

'освобождение памяти, занятой входными сеансами

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






Print "Интерполяция... ";


For h = 0 To 679 ' по всем высотам

	'Print_process_percent((h*100)/680)

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


	Next tau

Next h



Print_process_percent(1000)
Print "OK"







Print "Накопление по времени... ";

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

	as_file_str.acf = Callocate( as_file_str.nh, SizeOf(acf_struct) )   ' резервируем память для АКФ
	If as_file_str.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	/'
	as_file_str.param = Callocate( as_file_str.nh, SizeOf(param_struct) ) ' резервируем память для параметров
	If as_file_str.param = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
'/


	' размещаем данные по высотам
	For h = 0 To as_file_str.nh - 1 ' по высоте
		as_file_str.acf[h].n = h
		If pulseLength = 663 Then
			as_file_str.acf[h].h = seans1s_alt(h)
		Else
			as_file_str.acf[h].h = seans1s_alt_795(h)
		EndIf


		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].rc(tau) = 0
			For i = 0 To tNak-1
				as_file_str.acf[h].rc(tau) += seans_str_out(h).datCos(tau)[t+i]
			Next i
			as_file_str.acf[h].rc(tau) /= tNak
		Next tau

	Next h

	For h = 0 To as_file_str.nh - 1 ' по высоте
		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].rs(tau) = 0
		Next tau
	Next h


	For h = 0 To as_file_str.nh - 1 ' по высоте
		as_file_str.acf[h].pShort = 0
	Next h



	' определяем АКФ шума
	For tau = 0 To 18 ' по задержке
		as_file_str.rnc(tau) = 0
		For h = 500 To 599 ' по высоте
			as_file_str.rnc(tau) += as_file_str.acf[h].rc(tau)
		Next h
		as_file_str.rnc(tau) /= 100
	Next tau


	For tau = 0 To 18 ' по задержке
		as_file_str.rns(tau) = 0
	Next tau

	' отношение сигнал/шум
	For h = 0 To as_file_str.nh - 1 ' по высоте
		If as_file_str.rnc(0) <> 0 Then
			as_file_str.acf[h].q = as_file_str.acf[h].rc(0)/as_file_str.rnc(0)
		Else
			as_file_str.acf[h].q = 0
		EndIf
	Next h



	For h = 0 To as_file_str.nh - 1
		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].var(tau) = 0
		Next tau
	Next h

	If isVar <> 0 Then ' расчёт дисперсии

		' расчёт мат. ожидания АКФ сигнала

		ReDim As Double mean(0 To as_file_str.nh - 1, 0 To 18)

		For tau = 0 To 18 ' по задержке
			For h = 0 To as_file_str.nh - 1 ' по высоте
				mean(h, tau) = as_file_str.acf[h].rc(tau) - as_file_str.rnc(tau)
			Next h
		Next tau

		' расчёт дисперсии точек АКФ

		For h = 0 To as_file_str.nh - 1
			For tau = 0 To 18 ' по задержке

				Dim As Double noise(0 To tNak-1)

				For i = 0 To tNak-1
					noise(i) = 0
					For z As Integer = 500 To 599
						noise(i) += seans_str_out(z).datCos(tau)[t+i]
					Next z
					noise(i) /= 100
				Next i

				as_file_str.acf[h].var(tau) = 0
				For i = 0 To tNak-1
					as_file_str.acf[h].var(tau) += (mean(h, tau) - seans_str_out(h).datCos(tau)[t+i] + noise(i))^2
				Next i
				as_file_str.acf[h].var(tau) /= tNak

			Next tau
		Next h

	EndIf

	as_file_save( SEANS_DIR_OUT + DirectoryOutput + "/step1/"+ "AS" + DirectoryOutput + "." + EXT,  @as_file_str) ' запись в файл

	DeAllocate(as_file_str.acf)   ' освобождаем память с АКФ
	'	DeAllocate(as_file_str.param) ' освобождаем память с параметрами

	t += tStep
	seans_current += 1

Loop

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

