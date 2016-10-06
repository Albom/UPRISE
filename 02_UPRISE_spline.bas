
#Include Once "albom_lib.bi"	' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами

#Include "crt/stdlib.bi"

'''==============================================

Type seans_struct_in
	Dim seans 			As seans2_data
	Dim time_computer	As Integer
End Type



Type seans_struct_out
	Dim datCos(0 To 18)	As Double  Ptr 
	Dim datSin(0 To 18)	As Double  Ptr 
	Dim m						As Integer Ptr 
End Type



Type seans_struct_time
	Dim time_computer		As Integer Ptr 
	Dim date_				As ZString Ptr Ptr  
	Dim time_				As ZString Ptr Ptr
End Type


'''==============================================

Declare Sub LoadFiles(ByVal Directory As String)
Declare Function seans_struct_time_compare cdecl (elem1 As Any Ptr, elem2 As Any Ptr) As Integer

'''==============================================

Dim Shared As ZString Ptr lst  ' список файлов

Dim Shared As seans_struct_in Ptr seans_str_in 'массив структур данных из сеансов
Dim Shared As Integer seans_loaded   ' сеансов загружено

Dim Shared As Integer seans_out_num   ' количество выходных сеансов  

Dim Shared As seans_struct_out seans_str_out(0 To 679) 'массив структур данных из сеансов
Dim Shared As seans_struct_time seans_str_time 'структур данных для времени (которое хранится отдельно от seans_str_out для экономии места)

Dim As Integer i, j
Dim As Integer t, h, tau

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_IN = "./in/"
Dim As String SEANS_DIR_OUT = "./out/"
Dim As String EXT
Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim As Integer Day1, Month1, Year1, Hour1, Minute1, Second1

Dim As Double Ptr in_x, in_y, out_x, out_y, trand

Dim Shared As Double tmp_d(0 To 1000)
Dim As Integer n1, n2
Dim As Double var1, var2

Dim Shared As Double razr(0 To 350)

Dim Shared As Double R0DAC_1(0 To 18) ' 0 АЦП 1-го косинусного канала 
Dim Shared As Double R0DAC_2(0 To 18) ' 0 АЦП 1-го синусного канала
Dim Shared As Double R0DAC_3(0 To 18) ' 0 АЦП 2-го косинусного канала
Dim Shared As Double R0DAC_4(0 To 18) ' 0 АЦП 2-го синусного канала

Dim As as_file_struct as_file_str ' буфер для выходного файла

'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1    

Cls 


Dim As String razr_filename
Dim As Integer file

file = FreeFile()
Open "config.dat" For Input As #file
Input #file, razr_filename
Close #file


If razr_load(razr_filename, @razr(0), 330) = 0 Then ' загрузка разрядника
	PrintErrorToLog(ErrorRazrNotLoaded, __FILE__, __LINE__)
	End
EndIf


Cls
Color 11
Print "UPRISE version 1.0 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print 
Color 7
Print "Spline - программа заполнения недостающих участков данных"
Print "автор: Александр Богомаз, н.с. Института ионосферы"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

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
		

		seans_str_out(h).datSin(tau) = Callocate( seans_out_num, SizeOf(Double) )
		If seans_str_out(h).datSin(tau) = NULL Then
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

' формирование АКФ сигнала НР + шума (вычитание шума производится после интерполяции!!!)

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
					R0DAC_1(tau) = CDbl(seans_str_in[t].seans.dat01(h))*CDbl(seans_str_in[t].seans.dat01(h+tau))/1463.0
					R0DAC_3(tau) = CDbl(seans_str_in[t].seans.dat02(h))*CDbl(seans_str_in[t].seans.dat02(h+tau))/1463.0
					R0DAC_2(tau) = CDbl(seans_str_in[t].seans.dat01(h))*CDbl(seans_str_in[t].seans.dat02(h+tau))/1463.0
					R0DAC_4(tau) = CDbl(seans_str_in[t].seans.dat02(h))*CDbl(seans_str_in[t].seans.dat01(h+tau))/1463.0
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
				seans_str_out(h).datCos(tau)[i] = ( CDbl(seans_str_in[t].seans.dat1(h, tau)) - R0DAC_1(tau) ) + ( CDbl(seans_str_in[t].seans.dat3(h, tau)) - R0DAC_3(tau) )
				seans_str_out(h).datSin(tau)[i] = ( CDbl(seans_str_in[t].seans.dat4(h, tau)) - R0DAC_4(tau) ) - ( CDbl(seans_str_in[t].seans.dat2(h, tau)) - R0DAC_2(tau) ) 
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

Print "Интерполяция сплайнами... ";

' выделяем память на врЕменные массивы
in_x = Allocate(seans_out_num*SizeOf(Double))
If in_x = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf
	
in_y = Allocate(seans_out_num*SizeOf(Double))
If in_y = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

out_x = Allocate(seans_out_num*SizeOf(Double))
If out_x = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

out_y = Allocate(seans_out_num*SizeOf(Double))
If out_y = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

trand = Allocate(seans_out_num*SizeOf(Double))
If trand = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

For h = 0 To 679 ' по всем высотам
	
	Print_process_percent((h*100)/680)
	
	For tau = 0 To 18 ' по всем задержкам

		' 1) Для косинусного канала
		i = 0
		For t = 0 To seans_out_num-1 ' по времени

			If seans_str_out(h).m[t] = 1 Then
				in_x[i] = seans_str_time.time_computer[t]
				in_y[i] = seans_str_out(h).datCos(tau)[t]
				i += 1
			EndIf

		Next t

		If i < seans_out_num Then ' если на данной высоте для данной ординаты есть пропуски  

			' собственно аппроксимация сплайнами
			spline_bspline3_d(in_x, in_y, i, out_x, out_y, seans_out_num)

			For t = 0 To seans_out_num-1 ' по времени

				If seans_str_out(h).m[t] = 0 Then
					seans_str_out(h).datCos(tau)[t] = array_linear_d(seans_str_time.time_computer[t], out_x, out_y, seans_out_num)
				EndIf

			Next t 

		' получение тренда
		For t = 0 To seans_out_num-1-20 ' по времени
			trand[t+20/2] = 0
			For i = 0 To 19
				trand[t+20/2] += seans_str_out(h).datCos(tau)[t+i]
			Next i
			trand[t+20/2] /= 20
		Next t

		' заполнение начала и конца тренда		
		For t = 0 To 20/2-1 ' по времени
			trand[t] = stat_mean_d(@(seans_str_out(h).datCos(tau)[0]), 20/2)
			trand[seans_out_num-1-t] = stat_mean_d(@(seans_str_out(h).datCos(tau)[seans_out_num-20/2-1]), 20/2)
		Next t

		' добавление шума
		For t = 0 To seans_out_num-1 ' по времени

			If seans_str_out(h).m[t] = 0 Then
				
				n1 = 0
				For i = 0 To 30-1
					If t - i - 1 < 0 Then Exit For
					If n1 = 15 Then Exit For
					If seans_str_out(h).m[t-i-1] = 1 Then 
						tmp_d(n1) = seans_str_out(h).datCos(tau)[t-i-1]-trand[t-i-1]
						n1 += 1
					EndIf
				Next i

				If n1 > 4 Then 
					var1 = stat_variance_d(@tmp_d(0), n1)
				Else
					var1 = 0
				EndIf

				n2 = 0
				For i = 0 To 30-1
					If t + i + 1 > seans_out_num-1 Then Exit For
					If n2 = 12 Then Exit For
					If seans_str_out(h).m[t+i+1] = 1 Then 
						tmp_d(n2) = seans_str_out(h).datCos(tau)[t+i+1]-trand[t+i+1]
						n2 += 1
					EndIf
				Next i

				If n2 > 4 Then 
					var2 = stat_variance_d(@tmp_d(0), n2)
				Else
					var2 = 0
				EndIf

				If (var1 > 0) And (var2 > 0) Then
					seans_str_out(h).datCos(tau)[t] += Sqr ( (var1+var2)/2 ) * random_gauss()
				Else 
					If var1 > 0 Then
						seans_str_out(h).datCos(tau)[t] += Sqr(var1) * random_gauss()
					Else
						seans_str_out(h).datCos(tau)[t] += Sqr(var2) * random_gauss()
					EndIf
				EndIf 


			EndIf

		Next t


		EndIf


		' 2) Для синусного канала

		i = 0
		For t = 0 To seans_out_num-1 ' по времени

			If seans_str_out(h).m[t] = 1 Then
				in_x[i] = seans_str_time.time_computer[t]
				in_y[i] = seans_str_out(h).datSin(tau)[t]
				i += 1
			EndIf

		Next t

		If i < seans_out_num Then ' если на данной высоте для данной ординаты есть пропуски  

			' собственно аппроксимация сплайнами
			spline_bspline3_d(in_x, in_y, i, out_x, out_y, seans_out_num)

			For t = 0 To seans_out_num-1 ' по времени

				If seans_str_out(h).m[t] = 0 Then
					seans_str_out(h).datSin(tau)[t] = array_linear_d(seans_str_time.time_computer[t], out_x, out_y, seans_out_num)
				EndIf

			Next t 

		' получение тренда
		For t = 0 To seans_out_num-1-20 ' по времени
			trand[t+20/2] = 0
			For i = 0 To 19
				trand[t+20/2] += seans_str_out(h).datSin(tau)[t+i]
			Next i
			trand[t+20/2] /= 20
		Next t

		' заполнение начала и конца тренда		
		For t = 0 To 20/2-1 ' по времени
			trand[t] = stat_mean_d(@(seans_str_out(h).datSin(tau)[0]), 20/2)
			trand[seans_out_num-1-t] = stat_mean_d(@(seans_str_out(h).datSin(tau)[seans_out_num-20/2-1]), 20/2)
		Next t

		' добавление шума
		For t = 0 To seans_out_num-1 ' по времени
			
			If seans_str_out(h).m[t] = 0 Then
				
				n1 = 0
				For i = 0 To 30-1
					If t - i - 1 < 0 Then Exit For
					If n1 = 15 Then Exit For
					If seans_str_out(h).m[t-i-1] = 1 Then 
						tmp_d(n1) = seans_str_out(h).datSin(tau)[t-i-1]-trand[t-i-1]
						n1 += 1
					EndIf
				Next i
			
				If n1 > 4 Then 
					var1 = stat_variance_d(@tmp_d(0), n1)
				Else
					var1 = 0
				EndIf

				n2 = 0
				For i = 0 To 30-1
					If t + i + 1 > seans_out_num-1 Then Exit For
					If n2 = 12 Then Exit For
					If seans_str_out(h).m[t+i+1] = 1 Then 
						tmp_d(n2) = seans_str_out(h).datSin(tau)[t+i+1]-trand[t+i+1]
						n2 += 1
					EndIf
				Next i

				If n2 > 4 Then 
					var2 = stat_variance_d(@tmp_d(0), n2)
				Else
					var2 = 0
				EndIf

				If (var1 > 0) And (var2 > 0) Then ' если есть точки и слева, и справа
					seans_str_out(h).datSin(tau)[t] += Sqr ( (var1+var2)/2 ) * random_gauss()
				Else 
					If var1 > 0 Then
						seans_str_out(h).datSin(tau)[t] += Sqr(var1) * random_gauss()
					Else
						seans_str_out(h).datSin(tau)[t] += Sqr(var2) * random_gauss()
					EndIf
				EndIf 

			EndIf
			
		Next t


		EndIf



	Next tau
	
Next h


' освобождаем память врЕменных массивов
DeAllocate(in_x)
DeAllocate(in_y)
DeAllocate(out_x)
DeAllocate(out_y)
DeAllocate(trand)

Print_process_percent(1000)
Print "OK"

Print "Запись в файлы... ";


For t = 0 To seans_out_num-1-1 ' по времени
	
	Print_process_percent((t*100)/seans_out_num)
	
	EXT = Str(t+1)
	If t+1 < 1000 Then EXT = "0" + EXT
	If t+1 < 100  Then EXT = "0" + EXT
	If t+1 < 10   Then EXT = "0" + EXT
	

	as_file_str.filename = "AS" + DirectoryOutput + "." + EXT
	as_file_str.date_ = *seans_str_time.date_[t]
	as_file_str.time_ = *seans_str_time.time_[t]
	
	as_file_str.nseans = t+1
	as_file_str.tnak = 1

	as_file_str.fkr = 0.0

	as_file_str.nh = 680

	as_file_str.acf = Callocate( as_file_str.nh, SizeOf(acf_struct) )   ' резервируем память для АКФ
	If as_file_str.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
	
	as_file_str.param = Callocate( as_file_str.nh, SizeOf(param_struct) ) ' резервируем память для параметров
	If as_file_str.param = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	' размещаем данные по высотам
	For h = 0 To as_file_str.nh - 1 ' по высоте
		as_file_str.acf[h].n = h+1
		as_file_str.acf[h].h = seans2_altL(h) 
		
		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].rc(tau) = seans_str_out(h).datCos(tau)[t]
			as_file_str.acf[h].rs(tau) = seans_str_out(h).datSin(tau)[t]
		Next tau
	Next h

	' определяем АКФ шума
	For tau = 0 To 18 ' по задержке
		as_file_str.rnc(tau) = 0
		as_file_str.rns(tau) = 0		
		For h = 500 To 600 ' по высоте
			as_file_str.rnc(tau) += as_file_str.acf[h].rc(tau)
			as_file_str.rns(tau) += as_file_str.acf[h].rs(tau)
		Next h
		as_file_str.rnc(tau) /= 100
		as_file_str.rns(tau) /= 100
	Next tau

	' вычитаем АКФ шума
	For h = 0 To as_file_str.nh - 1 ' по высоте
		For tau = 0 To 18 ' по задержке
			as_file_str.acf[h].rc(tau) -= as_file_str.rnc(tau)
			as_file_str.acf[h].rs(tau) -= as_file_str.rns(tau)
		Next tau
	Next h

'Dim As Integer file
'file = FreeFile()
'Open "K-new.txt" For Output As #file

	' учёт разрядника
	For h = 0 To as_file_str.nh - 1 ' по высоте
		' вспомагательные локальные переменные (видны только в цикле) 
		Dim As Integer l1, l2 ' индексы
		Dim As Double  r1, r2 ' значения коэффициента передачи

'		Print #file, Using "##### "; seans2_altL(h), 

		l1 = h
		For tau = 0 To 18 ' по задержке

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
				as_file_str.acf[h].rc(tau) /= Sqr( r1*r2 )
				as_file_str.acf[h].rs(tau) /= Sqr( r1*r2 )
'				Print #file, Using "###.### "; Sqr( r1*r2 ), 
			Else
				as_file_str.acf[h].rc(tau) = 0
				as_file_str.acf[h].rs(tau) = 0
'				Print #file, Using "###.### "; 88,
			EndIf

		Next tau
'		Print #file, 
	Next h
'	break

	' вычисление отношения сигнал/шум
	For h = 0 To as_file_str.nh - 1 ' по высоте
		as_file_str.acf[h].q = as_file_str.acf[h].rc(0)/as_file_str.rnc(0)
	Next h

	as_file_save( SEANS_DIR_OUT + DirectoryOutput + "/step1/"+ "AS" + DirectoryOutput + "." + EXT,  @as_file_str) ' запись в файл

	DeAllocate(as_file_str.acf)   ' освобождаем память с АКФ
	DeAllocate(as_file_str.param) ' освобождаем память с параметрами


Next t

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
				seans2_load ( directory + "/" + filename, @(seans_str_in[seans_loaded].seans) )

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


Function seans_struct_time_compare cdecl (elem1 as any ptr, elem2 as any ptr) as Integer
	Return ( CPtr(seans_struct_in Ptr, elem1) -> time_computer ) - ( CPtr(seans_struct_in Ptr, elem2) -> time_computer ) 
End Function

