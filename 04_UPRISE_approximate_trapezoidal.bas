
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

Dim As Integer partrap

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

Dim As Integer t, tau, h
Dim As Integer z ' номер текущей высоты

Dim As as_file_struct	as_file_in
Dim As as_file_struct 	as_file_out

Dim As Integer h_out

Dim As Integer is_divide


'''==============================================

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Cls
Color 11
Print "UPRISE version 1.0 beta"
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print 
Color 7
Print "Approximate trapezoidal - программа высотной коррекции данных"
Print "автор: Александр Богомаз, н.с. Института ионосферы"
Print

Color 15

Input "Введите дату начала измерений (день месяц год): ", d_day, d_month, d_year
Input "Введите количество суток: ", d_ndays

Print
Input "Введите параметр трапецеидального суммирования: ", partrap
Print

'partrap = 0
'Input "Делить на количество слагаемых? (0 - нет, 1 - да): ", is_divide
is_divide=1

If (is_divide <> 0) And (is_divide <> 1) Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
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
		
		date_next(@d_day, @d_month, @d_year)
		
		If i = 0 Then 
			DirectoryOutput += directory + "-"
		EndIf
	EndIf
Next i
DirectoryOutput += directory


Print
Print "Подсчёт количества входных файлов... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"




Print "Суммирование по высоте... ";

seans_num_out = seans_num_in

MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step3")

For t = 0 To seans_num_out-1 ' по времени
	
	Print_process_percent((t*100)/seans_num_out)
		
	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext
	
	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext
	
	If as_file_load(filename, @as_file_in) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
	EndIf
	
	as_file_out.filename = as_file_in.filename
	as_file_out.date_    = as_file_in.date_ 
	as_file_out.time_    = as_file_in.time_ 
	as_file_out.nseans   = as_file_in.nseans 
	as_file_out.tnak     = as_file_in.tnak  
	
	For tau = 0 To 18  ' по задержке
		as_file_out.rnc(tau) = as_file_in.rnc(tau)
		as_file_out.rns(tau) = as_file_in.rns(tau)
	Next tau
	
	as_file_out.nh = as_file_in.nh-partrap-18-partrap
	
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


	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1
		
		as_file_out.acf[h_out].n = h_out+1
		as_file_out.acf[h_out].h = as_file_in.acf[h].h
		
		h_out += 1

	Next h 


	' 1) для косинусной составляющей
	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1

		For tau = 0 To 18
			
			as_file_out.acf[h_out].rc(tau) = 0
			For z = h-tau-partrap To h+partrap ' по высоте
				as_file_out.acf[h_out].rc(tau) += as_file_in.acf[z].rc(tau)
			Next z
			
			If is_divide = 1 Then
				as_file_out.acf[h_out].rc(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
			EndIf
		
		Next tau
		
		h_out += 1
		
	Next h 

	' 2) для синусной составляющей
	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1
		For tau = 0 To 18
			
			as_file_out.acf[h_out].rs(tau) = 0
			For z = h-tau-partrap To h+partrap ' по высоте
				as_file_out.acf[h_out].rs(tau) += as_file_in.acf[z].rs(tau)
			Next z
			
			If is_divide = 1 Then
				as_file_out.acf[h_out].rs(tau) /= tau+2*partrap+1 ' делить на количество слагаемых
			EndIf
		
		Next tau

		h_out += 1
	Next h 

	' Запись корректированной мощности и отношения с/ш, расчёт и запись отношения с/ш
	
	h_out = 0
	For h = 18+partrap To as_file_in.nh-partrap-1
	
		as_file_out.acf[h_out].pcorr = as_file_in.acf[h].pcorr
		as_file_out.acf[h_out].qcorr = as_file_in.acf[h].qcorr
		
		as_file_out.acf[h_out].q = as_file_out.acf[h_out].rc(0)/as_file_out.rnc(0)
		
		h_out += 1
		
	Next h 
	
	filename = SEANS_DIR_OUT+DirectoryOutput+"/step3/AS"+DirectoryOutput+"."+ext
	
	
	
	as_file_save(filename, @as_file_out)
	
	
		
	DeAllocate(as_file_out.acf)   ' освобождаем память с АКФ
	DeAllocate(as_file_out.param) ' освобождаем память с параметрами
	
	as_file_close(@as_file_in)
	
Next t

Print_process_percent(1000)
Print "OK"


Print
Print "OK"
break

'''==============================================
