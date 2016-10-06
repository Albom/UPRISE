	
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами

#Include "crt/stdlib.bi"

#Include "fbgfx.bi"			'Подключение графической библиотеки


'''==============================================

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf


'''==============================================

Dim As Integer i, j

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_OUT = "./out/"
Dim As String filename
Dim As String ext

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer seans_num_in
Dim As Integer seans_current
Dim As Integer seans_num_out

Dim As Integer Tnak  ' время накопления
Dim As Integer Tstep ' шаг по времени

Dim As Integer t, tau, h

Dim As as_file_struct Ptr	as_file_array
Dim As as_file_struct 		as_file_out

Dim As Integer symbol = 0 ' вспомогательная переменная для отображения процесса загрузки сеансов

Dim As Double  ambig(0 To 18, 0 To 44, 0 To 65)
Dim As Double  ambig_sum
Dim As Integer ambig_points = 25

Dim As Integer file

'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1

Cls
Color 11
Print "UPRISE version 1.0 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print 
Color 7
Print "Integrate - программа временного усреднения данных"
Print "автор: Александр Богомаз, н.с. Института ионосферы"
Print

Color 15
 
Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

Print 
Input "Введите время накопления (в мин): ", Tnak
Input "Введите шаг перемещения окна (в мин): ", Tstep


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
		
		date_next(@d_day, @d_month, @d_year)
		
		If i = 0 Then 
			DirectoryOutput += directory + "-"
		EndIf
	EndIf
Next i
DirectoryOutput += directory

Print
Print "Подсчёт количества входных файлов... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step1")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print #1, Str(seans_num_in)+" files loaded"

Print "OK"

seans_num_out = 0
t = 0
Do Until t + Tnak > seans_num_in
	t += Tstep
	seans_num_out += 1
Loop

If seans_num_out < 1 Then
	PrintErrorToLog(ErrorNoOutputFiles, __FILE__, __LINE__)
	End
EndIf

MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step2")


as_file_array = Callocate(Tnak, SizeOf(as_file_struct))
If as_file_array = NULL Then
	PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
	End
EndIf

Print 
Print "Загрузка ДФН... ";


file = FreeFile()
Open "./ambig/no/ambig00.dat" For Input As #file
If Err() <> 0 Then
	PrintErrorToLog(ErrorAFunction, __FILE__, __LINE__)
	End
EndIf

For h = 0 To 65
	For tau = 0 To 44
	
		Input #file, ambig(0, tau, h)
	
	Next tau
Next h

Close #file

ambig_sum = 0
For h = 0 To ambig_points
	ambig_sum += ambig(0, 44/2,h)
Next h

Print "OK"

Print "Накопление данных по времени... ";

seans_current = 0
t = 0
Do Until t + Tnak > seans_num_in
	
	Print_process_percent((seans_current*100)/seans_num_out)
	
	For i = 0 To Tnak-1 ' по времени
		ext = Str(t+i+1)
		If t+i+1 < 1000 Then ext = "0" + ext
		If t+i+1 < 100  Then ext = "0" + ext
		If t+i+1 < 10   Then ext = "0" + ext
		
		filename = SEANS_DIR_OUT+DirectoryOutput+"/step1/AS"+DirectoryOutput+"."+ext
		
		If as_file_load(filename, @(as_file_array[i])) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
			PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
			End
		EndIf
	Next i

	ext = Str(seans_current+1)
	If seans_current+1 < 1000 Then ext = "0" + ext
	If seans_current+1 < 100  Then ext = "0" + ext
	If seans_current+1 < 10   Then ext = "0" + ext
	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext

	as_file_out.filename = "AS"+DirectoryOutput+"."+ext
	as_file_out.date_ = as_file_array[Tnak\2].date_
	as_file_out.time_ = as_file_array[Tnak\2].time_
	as_file_out.nseans = t+1
	as_file_out.tnak = Tnak


	' Накапливаем АКФ шума
	For tau = 0 To 18  ' по задержке

		as_file_out.rnc(tau) = 0
		as_file_out.rns(tau) = 0
		For i = 0 To Tnak-1 ' по времени
			as_file_out.rnc(tau) += as_file_array[i].rnc(tau)
			as_file_out.rns(tau) += as_file_array[i].rns(tau)
		Next i
		as_file_out.rnc(tau) /= Tnak
		as_file_out.rns(tau) /= Tnak

	Next tau

	as_file_out.nh = as_file_array[0].nh

	as_file_out.acf = Callocate(as_file_out.nh, SizeOf(acf_struct) )
	If as_file_out.acf = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	as_file_out.param = Callocate(as_file_out.nh, SizeOf(param_struct) )
	If as_file_out.param = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	For h = 0 To as_file_out.nh-1 ' по высотам
		
		as_file_out.acf[h].n = as_file_array[0].acf[h].n
		as_file_out.acf[h].h = as_file_array[0].acf[h].h

		' накапливаем АКФ сигнала
		' 1) для косинусной составляющей 
		For tau = 0 To 18  ' по задержке
			as_file_out.acf[h].rc(tau) = 0
			For i = 0 To Tnak-1 ' по времени
				as_file_out.acf[h].rc(tau) += as_file_array[i].acf[h].rc(tau)
			Next i
			as_file_out.acf[h].rc(tau) /= Tnak
		Next tau


		' 2) для синусной составляющей
		For tau = 0 To 18  ' по задержке
			as_file_out.acf[h].rs(tau) = 0
			For i = 0 To Tnak-1 ' по времени
				as_file_out.acf[h].rs(tau) += as_file_array[i].acf[h].rs(tau)
			Next i
			as_file_out.acf[h].rs(tau) /= Tnak
		Next tau


		' рассчитываем отношение сигнал/шум
		as_file_out.acf[h].q = as_file_out.acf[h].rc(0)/as_file_out.rnc(0)

	Next h 

	' Коррекция профиля мощности
	
	' !!! Шаг 1 - сдвиг экспериментального профиля P на 0,6*Tи
	
	Dim As Double Ptr p_06Tu
	Dim As Double Ptr h_06Tu
	
	p_06Tu = Callocate(as_file_out.nh, SizeOf(Double))
	If p_06Tu = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
		
	h_06Tu = Callocate(as_file_out.nh, SizeOf(Double))
	If h_06Tu = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
	
	For h = 0 To as_file_out.nh-13
		p_06Tu[h] = as_file_out.acf[h+13].rc(0)
		h_06Tu[h] = as_file_out.acf[h].h 
	Next h

	' !!! Шаг 2 - интерполяция сдвинутого профиля сплайном

	' поиск максимума
	
	Dim As Double 		p_06Tu_max
	Dim As Double 		h_06Tu_max
	Dim As Double 		n_06Tu_max
	
	p_06Tu_max = p_06Tu[0]
	For h = 1 To as_file_out.nh-1
		If p_06Tu[h] > p_06Tu_max Then 
			p_06Tu_max = p_06Tu[h]
			h_06Tu_max = h_06Tu[h]
			n_06Tu_max = h
		EndIf
	Next h


	' подготовка сплайнов
	
	Const As Integer num_before = 100
	Const As Integer num_after  = 300
		
	Dim As Double p_spline_before(0 To num_before)
	Dim As Double h_spline_before(0 To num_before)

	Dim As Double p_spline_after(0 To num_after)
	Dim As Double h_spline_after(0 To num_after)
	
	Dim As Double Ptr p_line
	Dim As Double Ptr h_line
	
	p_line = Callocate(as_file_out.nh, SizeOf(Double))
	If p_line = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
		
	h_line = Callocate(as_file_out.nh, SizeOf(Double))
	If h_line = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf
	
	For h = 5 To 100
		p_spline_before(h) = p_06Tu[n_06Tu_max+10+h*2]
		h_spline_before(h) = h_06Tu[n_06Tu_max+10+h*2]
	Next h
	
	h_spline_before(0) = h_06Tu[n_06Tu_max-10]
	h_spline_before(1) = h_06Tu[n_06Tu_max-5] 
	h_spline_before(2) = h_06Tu[n_06Tu_max]
	h_spline_before(3) = h_06Tu[n_06Tu_max+5]
	h_spline_before(4) = h_06Tu[n_06Tu_max+10]  

	Dim As Double k0, k1, k2, k3, k4
	Dim As Double k0_c, k1_c, k2_c, k3_c, k4_c
	Dim As Double d, d_c

	Dim As Double Ptr p_work

	p_work = Callocate(as_file_out.nh, SizeOf(Double))
	If p_work = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	k0 = 1
	k4 = 1

	' поиск 
	d_c = 1e50
	'For k0 = 0.8 To 1.6 Step 0.2
	For k1 = 0.8 To 1.6 Step 0.1
	For k2 = 0.8 To 1.6 Step 0.1
	For k3 = 0.8 To 1.6 Step 0.1
	'For k4 = 0.8 To 1.6 Step 0.2

		p_spline_before(0) = p_06Tu[n_06Tu_max-10] * k0
		p_spline_before(1) = p_06Tu[n_06Tu_max-5]  * k1
		p_spline_before(2) = p_06Tu[n_06Tu_max]    * k2
		p_spline_before(3) = p_06Tu[n_06Tu_max+5]  * k3
		p_spline_before(4) = p_06Tu[n_06Tu_max+10] * k4

		' интерполяция сплайном
		spline_bspline3_d(@h_spline_before(0), @p_spline_before(0), num_before, @h_spline_after(0), @p_spline_after(0), num_after)

		' линейная интерполяция
		For h = 0 To as_file_out.nh-1
			h_line[h] = h_06Tu[h]
			p_line[h] = array_linear_d ( h_line[h], @h_spline_after(0), @p_spline_after(0), num_after)
		Next h

		' Учёт ДФН - получение профиля, который будет сравниваться с экспериментальным
		For i = ambig_points+20 to as_file_out.nh-1
			p_work[i] = 0
			For j = 0 To 50
				p_work[i] += p_line[i-j]*ambig(0, 22, j)
			Next j
			p_work[i] /= ambig_sum
		Next i

		' Сравнение рабочего профиля с экспериментальным
		d = 0 
		For h = n_06Tu_max-20 to n_06Tu_max+20
			d += (p_work[h] - as_file_out.acf[h].rc(0))^2
		Next h

		If d < d_c Then
			d_c = d
			k0_c = k0
			k1_c = k1
			k2_c = k2
			k3_c = k3
			k4_c = k4
		EndIf

	'Next k4
	Next k3
	Next k2
	Next k1
	'Next k0

	p_spline_before(0) = p_06Tu[n_06Tu_max-10] * k0_c
	p_spline_before(1) = p_06Tu[n_06Tu_max-5]  * k1_c
	p_spline_before(2) = p_06Tu[n_06Tu_max]    * k2_c
	p_spline_before(3) = p_06Tu[n_06Tu_max+5]  * k3_c
	p_spline_before(4) = p_06Tu[n_06Tu_max+10] * k4_c

	' интерполяция сплайном
	spline_bspline3_d(@h_spline_before(0), @p_spline_before(0), num_before, @h_spline_after(0), @p_spline_after(0), num_after)

	' линейная интерполяция
	For h = 0 To as_file_out.nh-1
		h_line[h] = h_06Tu[h]
		p_line[h] = array_linear_d ( h_line[h], @h_spline_after(0), @p_spline_after(0), num_after)
	Next h


	' "поднимаем" восстановленный профиль на 0,6Tи
	' забиваем начало нулями 
	For h = 0 To 13
		as_file_out.acf[h].pcorr = 0
		as_file_out.acf[h].qcorr = 0
	Next h

	' записываем выходные данные 
	For h = 0 To as_file_out.nh-1-13
		as_file_out.acf[h+13].pcorr = p_line[h]
		as_file_out.acf[h+13].qcorr = p_line[h] / as_file_out.rnc(0)
	Next h

	DeAllocate(p_06Tu)
	DeAllocate(h_06Tu)

	DeAllocate(p_work)

	as_file_save(filename, @as_file_out)

	DeAllocate(as_file_out.acf)   ' освобождаем память с АКФ
	DeAllocate(as_file_out.param) ' освобождаем память с параметрами

	For i = 0 To Tnak-1 ' по времени
		as_file_close( @(as_file_array[i]) ) ' освобождаем память, выделенную под Tnak сеансов
	Next i

	t += Tstep
	seans_current += 1

Loop	

Print_process_percent(1000)
Print "OK"

DeAllocate(as_file_array)

Print
Print "OK"
break



'''==============================================
