
Type acf_struct 'структура АКФ
	Dim	n							As Integer			' номер высоты
	Dim	h							As Double			' высота в км
	Dim	q							As Double			' отношение с/ш
	Dim	qcorr						As Double			' скорректированное отношение с/ш
	Dim	pcorr						As Double			' скорректированная мощность
	Dim	pshort					As Double			' мощность по короткому импульсу
	Dim	rc(0 To 18)				As Double			' косинусная составляющая АКФ НР сигнала
	Dim	rs(0 To 18)				As Double			' синусная составляющая АКФ НР сигнала
	Dim	var(0 To 18)			As Double			' дисперсия точек АКФ
End Type

Type param_struct 'структура параметров
	Dim	ti							As Double			' температура ионов
	Dim	te							As Double			' температура электронов
	Dim	h							As Double			' относительное содержание ионов водорода
	Dim	he							As Double			' относительное содержание ионов гелия
	Dim	o							As Double			' относительное содержание ионов кислорода
	Dim	m							As Double			' относительное содержание тяжёлых ионов
	Dim	ne							As Double			' абсолютное значение концентрации электронов
	Dim	vz							As Double			' вертикальная скорость движения плазмы
End Type

Type short_struct
	Dim	n							As Integer
	Dim	h							As Double
	Dim	q							As Double
	Dim	p							As Double
End Type

Type as_file_struct 'структура данных выходных сеансов
	Dim	state						As String
	Dim	filename					As ZString*256
	Dim	date_						As ZString*16
	Dim	time_						As ZString*16
	Dim	nseans					As Integer				' номер сеанса
	Dim	tnak						As Integer				' время накопления
	Dim	fkr						As Double				' критическая частота
	Dim	rnc(0 To 18)			As Double				' косинусная составляющая АКФ шума
	Dim	rns(0 To 18)			As Double				' синусная составляющая АКФ шума
	Dim	nh							As Integer				' количество высотных участков
	Dim	acf						As acf_struct Ptr
	'	Dim	param						As param_struct Ptr
	'	Dim	pn_short					As Double
	'	Dim	nh_short					As Integer
	'	Dim	p_short					As short_struct Ptr
End Type

Function as_file_load(ByRef Filename As String, ByRef as_file_str As as_file_struct Ptr) As Integer

	Dim As Integer file
	Dim As Integer tau, h

	If as_file_str->state = "OPENED" Then
		Return 0
	EndIf

	file = FreeFile
	Open Filename For Input As #file

	Input #file, as_file_str->filename
	as_file_str->filename = Mid( as_file_str->filename, InStrRev (as_file_str->filename, "/")+1 )
	Input #file, as_file_str->date_
	Input #file, as_file_str->time_
	Input #file, as_file_str->nseans
	Input #file, as_file_str->tnak
	Input #file, as_file_str->fkr
	/'
	Input #file, as_file_str->pn_short
	Input #file, as_file_str->nh_short

	as_file_str->p_short = Allocate(as_file_str->nh_short*SizeOf(short_struct))
	If as_file_str->p_short = NULL Then
		Return 0
	EndIf

	For h = 0 To as_file_str->nh_short-1 ' по высоте
		Input #file, as_file_str->p_short[h].n
		Input #file, as_file_str->p_short[h].h
		Input #file, as_file_str->p_short[h].q
		Input #file, as_file_str->p_short[h].p
	Next h
'/
	For tau = 0 To 18 ' по ординате
		Input #file, as_file_str->rnc(tau)
	Next tau

	For tau = 0 To 18 ' по ординате
		Input #file, as_file_str->rns(tau)
	Next tau

	Input #file, as_file_str->nh

	as_file_str->acf = Allocate(as_file_str->nh*SizeOf(acf_struct))
	If as_file_str->acf = NULL Then
		Return 0
	EndIf
	/'
	as_file_str->param = Allocate(as_file_str->nh*SizeOf(param_struct))
	If as_file_str->param = NULL Then
		Return 0
	EndIf
'/
	For h = 0 To as_file_str->nh-1 ' по высоте
		Input #file, as_file_str->acf[h].n
		Input #file, as_file_str->acf[h].h
		Input #file, as_file_str->acf[h].q
		Input #file, as_file_str->acf[h].qcorr
		Input #file, as_file_str->acf[h].pcorr
		
'		Input #file, as_file_str->acf[h].pshort

		For tau = 0 To 18 ' по ординате
			Input #file, as_file_str->acf[h].rc(tau)
		Next tau

		For tau = 0 To 18 ' по ординате
			Input #file, as_file_str->acf[h].rs(tau)
		Next tau

		For tau = 0 To 18 ' по ординате
			Input #file, as_file_str->acf[h].var(tau)
		Next tau

		/'
		Input #file, as_file_str->param[h].ti
		Input #file, as_file_str->param[h].te
		Input #file, as_file_str->param[h].h
		Input #file, as_file_str->param[h].he
		Input #file, as_file_str->param[h].o
		Input #file, as_file_str->param[h].m
		Input #file, as_file_str->param[h].ne
		Input #file, as_file_str->param[h].vz
'/

	Next h

	Close #file

	as_file_str->state = "OPENED"
	Return 1

End Function


Sub as_file_close(ByRef as_file_str As as_file_struct Ptr)

	If as_file_str->state = "OPENED" Then

		DeAllocate(as_file_str->acf)
		'		DeAllocate(as_file_str->param)
		'		DeAllocate(as_file_str->p_short)

		as_file_str->state = "CLOSED"

	EndIf

End Sub


Sub as_file_save(ByRef Filename As String, ByRef as_file_str As as_file_struct Ptr)
	Dim As Integer file
	Dim As Integer tau, h
	Dim As ZString Ptr buffer

	buffer = Allocate(1*1024*1024) ' выделить на буфер файла 1 Мбайт
	If buffer = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	buffer_clear(buffer)
	
	buffer_add_s(buffer, as_file_str->filename, "%s") ' записываем имя файла
	buffer_newline(buffer)

	buffer_add_s(buffer, as_file_str->date_, "%s") ' записываем дату
	buffer_newline(buffer)

	buffer_add_s(buffer, as_file_str->time_, "%s") ' записываем время
	buffer_newline(buffer)

	buffer_add_i(buffer, @(as_file_str->nseans), 1, "%d") ' записываем номер сеанса
	buffer_newline(buffer)

	buffer_add_i(buffer, @(as_file_str->tnak), 1, "%d") ' записываем время накопления
	buffer_newline(buffer)

	buffer_add_d(buffer, @(as_file_str->fkr), 1, "%-10.5lf") ' записываем критическую частоту
	buffer_newline(buffer)

	buffer_add_d(buffer, @(as_file_str->rnc(0)), 19, "%-15.0lf") ' запись АКФ шума (cos)

	buffer_add_d(buffer, @(as_file_str->rns(0)), 19, "%-15.0lf") ' запись АКФ шума (sin)

	buffer_newline(buffer)

	buffer_add_i(buffer, @(as_file_str->nh), 1, "%d") ' записываем число высот
	buffer_newline(buffer)

	For h = 0 To as_file_str->nh - 1

		buffer_add_i(buffer, @(as_file_str->acf[h].n), 1, "%-7d") ' записываем номер высоты
		buffer_add_d(buffer, @(as_file_str->acf[h].h), 1, "%10.3lf") ' запись высоты в км

		buffer_add_d(buffer, @(as_file_str->acf[h].q), 1, "%10.3lf") ' запись отношения с/ш
		buffer_add_d(buffer, @(as_file_str->acf[h].qcorr), 1, "%10.3lf") ' запись откорректированного отношения с/ш

		buffer_add_d(buffer, @(as_file_str->acf[h].pcorr), 1, "%15.0lf") ' запись откорректированной мощности
		
'		buffer_add_d(buffer, @(as_file_str->acf[h].pshort),1, "%15.0lf") ' запись мощности по короткому импульсу

		buffer_add_d(buffer, @(as_file_str->acf[h].rc(0)), 19, "%15.0lf") ' запись АКФ сигнала НР (cos)
		buffer_add_d(buffer, @(as_file_str->acf[h].rs(0)), 19, "%15.0lf") ' запись АКФ сигнала НР (sin)
		
		buffer_add_d(buffer, @(as_file_str->acf[h].var(0)),19, "%15.0lf") ' запись дисперсии точек АКФ

		/'
		buffer_add_d(buffer, @(as_file_str->param[h].ti), 1, "%10.3lf") ' запись Ti
		buffer_add_d(buffer, @(as_file_str->param[h].te), 1, "%10.3lf") ' запись Te
		buffer_add_d(buffer, @(as_file_str->param[h].h),  1, "%10.3lf") ' запись H
		buffer_add_d(buffer, @(as_file_str->param[h].he), 1, "%10.3lf") ' запись He
		buffer_add_d(buffer, @(as_file_str->param[h].o),  1, "%10.3lf") ' запись O
		buffer_add_d(buffer, @(as_file_str->param[h].m),  1, "%10.3lf") ' запись M
		buffer_add_d(buffer, @(as_file_str->param[h].ne), 1, "%10.3lf") ' запись Ne
		buffer_add_d(buffer, @(as_file_str->param[h].vz), 1, "%10.3lf") ' запись Vz
'/
		buffer_newline(buffer)

	Next h

	'	file_creat(filename)
	'	file_add_s(filename, buffer, "%s")

	file_creat_and_add_s(@filename[0], buffer)

	DeAllocate (buffer)


End Sub


Function as_file_test(ByRef Filename As String) As Integer
	Dim As Integer f
	Dim As String signature

	f = FreeFile
	Open Filename For Input As #f
	If Err>0 Then
		Return 0
	EndIf

	Input #f, signature
	Close #f

	If Mid(signature, 1, 2) = "AS" Then
		Return 1
	Else
		Return 0
	EndIf

End Function



'''==============================================

Function AS_File_Num(ByVal Directory As String) As Integer
	Dim As ZString Ptr lst  ' список файлов
	Dim As Integer lst_len  ' длина списка файлов
	Dim As Integer seans_num

	Dim As Integer i

	Dim As ZString*256 filename

	lst = Allocate (2*1024*1024*SizeOf(Byte)) ' выделить на список файлов 2 Мбайта
	If lst = NULL Then
		PrintErrorToLog(ErrorNotEnoughMemory, __FILE__, __LINE__)
		End
	EndIf

	lst_len = filelist_get(Directory, lst)

	seans_num = 0
	For i = 0 To lst_len-1
		filelist_get_filename(lst, @filename, i) 'получить имя файла из списка
		If as_file_test(Directory+"/"+filename) > 0 Then
			seans_num += 1
		EndIf
	Next i

	DeAllocate (lst)

	Return seans_num

End Function
