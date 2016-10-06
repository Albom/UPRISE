

Enum ErrorType 'Типы ошибок
	ErrorLib						' Ошибка типа "Библиотека не найдена"
	ErrorFunc					' Ошибка типа "Функция не найдена"
	ErrorSeansNotLoaded		' Ни один сеанс не загружен
	ErrorNotEnoughMemory		' Не хватает памяти
	ErrorRazrNotLoaded		' Файл разрядника не загружен
	ErrorNoOutputFiles		' Нет файлов для вывода
	ErrorLoadASFile			' Ошибка загрузки AS-файла
	ErrorInputData			 	' Неверные входные данные
	ErrorFortranLib			' Невозможно загрузить библиотеку АКФ
	ErrorAFunction				' Ошибка ДФН
	ErrorFilter					' Ошибка файла фильтра
	ErrorConfig					' Ошибка файла конфига
	ErrorFlip					' Ошибка загрузки файлов FLIP
End Enum



Sub PrintErrorToLog( _ 
					ByVal	ErrNum		As	Integer, _
					ByVal	FileName	As String, _
					ByVal	StrNum	As Integer _
					)

	Dim err_message As ZString * 256 'Строка с описанием ошибки
	Dim FileHandle As Integer 'Дескриптор файла 
	
	Select Case ErrNum
		Case ErrorLib
			err_message = "Ошибка загрузки динамической библиотеки"
		Case ErrorFunc
			err_message = "Ошибка загрузки функции"
		Case ErrorSeansNotLoaded
			err_message = "Ни один сеанс не загружен"
		Case ErrorNotEnoughMemory
			err_message = "Не хватает памяти"
		Case ErrorRazrNotLoaded
			err_message = "Файл разрядника не загружен"
		Case ErrorNoOutputFiles
			err_message = "Нет файлов для вывода"
		Case ErrorLoadASFile
			err_message = "Ошибка загрузки AS-, BS- или CS- файла"
		Case ErrorInputData
			err_message = "Неверные входные данные"
		Case ErrorFortranLib
			err_message = "Невозможно загрузить библиотеку АКФ"
		Case ErrorAFunction
			err_message = "Ошибка в двумерной функции неопределённости (ДФН)"
		Case ErrorFilter
			err_message = "Ошибка загрузки файла АКФ ИХ фильтра"
		Case ErrorConfig
			err_message = "Не удалось открыть конфигурационный файл"
		Case ErrorFlip
			err_message = "Ошибка загрузки файлов из модели FLIP"
	End Select
	FileHandle = FreeFile 'Присваиваем дескриптору свободное значение 
	Open "Error_Log_UPRISE.txt" For Append As #FileHandle
	Print #FileHandle, Date; ", "; Time; ". "; err_message; ". Файл: ";  Mid(FileName, InStrRev(FileName, "\")+1); ". Строка: "; StrNum; ". Программа собрана: "; __DATE__
	Close #FileHandle

End Sub
