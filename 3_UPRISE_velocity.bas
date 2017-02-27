
#Include Once "albom_lib.bi"	   ' Описание библиотеки "albom.dll"
#Include Once "albom_log.bi"		' Подключение лога
#Include Once "albom_as_file.bi"	' Описание структур и процедур для работы с AS-файлами
#Include Once "albom_version.bi"

#Include "crt/stdlib.bi"

#Include "fbgfx.bi"			'Подключение графической библиотеки

#Include "window9.bi"
#Include "dir.bi"


'''==============================================

#If __FB_LANG__ = "fb"
Using FB 'для перехода в полноэкранный режим монитора
#EndIf


'''==============================================

Const LAMBDA = 1.8987
Const DELTA_TAU = 30.555e-6
Const DELTA_TAU_INTERPOLATED = 1e-6

'''==============================================

Dim As Integer file, fileSpectrumBefore, fileSpectrumAfter, fileRcosBefore, fileRsinBefore, fileRcosAfter, fileRsinAfter

Dim As Integer kMax, kMin
Dim As Integer nh
Dim As Integer Hmin, Hmax, Hstep

Dim As Integer i, j

Dim As Integer d_month, d_year, d_day, d_ndays
Dim As String s_year, s_month, s_day

Dim As String SEANS_DIR_OUT = "./out/"
Dim As String filename
Dim As String ext

Dim As String Directory
Dim As String DirectoryOutput

Dim As Integer seans_num_in


Dim As Integer t, h, tau

Dim As Double r2(0 To 100)


Dim As as_file_struct	as_file_in

Dim As Integer num_algo

Dim As Integer isSpectrum = 0
Dim As Integer isSpectrumMod = 0

Dim As Integer isVelocityTrap = 0

Dim As Integer isAcf = 0

Dim As Double limit

Dim As Double limit_vz

'''==============================================

Hmin = 55
Hmax = 145
Hstep = 4

SetEnviron("fbgfx=GDI")

Screen 20
#Include Once "albom_font.bi"

Open Err For Output As #1

Cls

Color 11
Print "UPRISE version " + UPRISE_VERSION
Print "(Unified Processing of the Results of Incoherent Scatter Experiments)"
Print
Color 7
Print "Velocity - программа расчёта скорости движения плазмы"
Print "(c) Богомаз А.В., Котов Д.В. (Институт ионосферы)"
Print
Print "================================"
Print "Программа собрана " + Mid(__DATE__, 4, 2)+"."+Mid(__DATE__, 1, 2)+"."+Mid(__DATE__, 7, 4)
Print "================================"

Color 15


file = FreeFile()
Open "config_velocity.dat" For Input As #file
If Err() <> 0 Then
	Input "Hmin: ", Hmin
	Input "Hmax: ", Hmax
	Input "Hstep: ", Hstep
Else
	Input #file, Hmin
	Input #file, Hmax
	Input #file, Hstep
	Input #file, isSpectrum
	Input #file, isSpectrumMod
	Input #file, isVelocityTrap
	Input #file, isAcf
	Input #file, kMin, kMax
	Input #file, num_algo
	Input #file, limit
	Input #file, limit_vz
EndIf

If (Hmin < 0) Or (Hmax < Hmax) Or (Hstep < 1)  Then
	PrintErrorToLog(ErrorInputData, __FILE__, __LINE__)
	End
EndIf


Print
Color 12
Print "Проверьте параметры программы. При необходимости внесите исправления в config_velocity.dat и перезапустите программу."
Print
Color 15
Print "Номер минимальной высоты Hmin: "; Hmin
Print "Номер максимальной высоты Hmax: "; Hmax
Print "Шаг по высоте Hstep: "; Hstep
Print
Print "Номера минимальной и максимальной задержки: kMin = "; kMin; ", kMax = "; kMax
Print "Номер алгоритма: "; num_algo
Print "Ограничение: "; limit
Print
Print "Ограничение при записи в файлы: "; limit_vz
Print

If isSpectrum <> 0 Then
	Print "+ Расчёт спектра."
EndIf
If (isSpectrum <> 0) And (isSpectrumMod <> 0) Then
	Print "+ Изменение спектра."
EndIf
If (isSpectrum <> 0) And (isSpectrumMod <> 0) And (isVelocityTrap <> 0) Then
	Print "+ Расчёт скорости по изменённому спектру (смещение трапеции)."
EndIf
If isAcf <> 0 Then
	Print "+ Вывод АКФ в файл."
EndIf
Print
Color 12
Print "Нажмите Enter"


Dim As Integer key
Do
	key = GetKey()
Loop Until key = KEY_ENTER

Cls
Color 11

Print "Результаты обработки, находящиеся в папке " + Chr(34) + "out" + Chr(34) + ":"
Color 10
Dim As String fn
fn = Dir("./out/*", fbDirectory)
While Len(fn) > 0
	fn = Dir()
	If (Len(fn)=13) And (Mid(fn, 7, 1)="-") Then
		Print fn;"   ";
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







If as_file_load(SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+".0001", @as_file_in) = 0 Then ' загружаем файл (при этом выделяется память, поэтому её необходимо после освобождать)
	PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
	End
EndIf

nh = as_file_in.nh ' получаем число высот

ReDim Shared As Integer Hkm(0 To nh-1)

For h = 0 To nh-1
	Hkm(h) = as_file_in.acf[h].h
Next h

as_file_close( @as_file_in ) ' освобождаем память, выделенную на сеанс







Print
Print "Подсчёт количества входных файлов... ";

seans_num_in = AS_File_Num(SEANS_DIR_OUT+DirectoryOutput+"/step2")
If seans_num_in < 1 Then
	PrintErrorToLog(ErrorSeansNotLoaded, __FILE__, __LINE__)
	End
EndIf

Print "OK"






DeleteDir(SEANS_DIR_OUT + DirectoryOutput+"/step4", /'FOF_ALLOWUNDO Or '/FOF_NOCONFIRMATION Or FOF_SILENT)
MkDir(SEANS_DIR_OUT + DirectoryOutput+"/step4")







file = FreeFile
Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"T.txt" For Output As #file
Close #file







' Выделяем память для данных
Print "Выделение памяти для данных... ";
ReDim Shared As Double drift_d (0 To nh-1, 0 To seans_num_in-1)
ReDim Shared As Double time_d (0 To seans_num_in-1)
ReDim Shared As Double drift_d_trap (0 To nh-1, 0 To seans_num_in-1)
Print "OK"
Print #1, Str(seans_num_in)+" files loaded"
Print #1, "Free memory: "; Fre()\(1024*1024); " MBytes"







Print "Расчёт скорости движения плазмы... ";

ReDim Shared As Double r_cos(0 To 550), r_sin(0 To 550)

Dim As Double x(0 To 18)
For tau = 0 To 18
	x(tau) = tau*DELTA_TAU
Next

ReDim Shared As Double sp(0 To 10000)

For t = 0 To seans_num_in-1 ' по времени

	Print_process_percent((t*100)/seans_num_in)

	ext = Str(t+1)
	If t+1 < 1000 Then ext = "0" + ext
	If t+1 < 100  Then ext = "0" + ext
	If t+1 < 10   Then ext = "0" + ext

	filename = SEANS_DIR_OUT+DirectoryOutput+"/step2/AS"+DirectoryOutput+"."+ext

	If as_file_load(filename, @as_file_in) = 0 Then ' загружаем файлы (при этом выделяется память, поэтому её необходимо после освобождать)
		PrintErrorToLog(ErrorLoadASFile, __FILE__, __LINE__)
		End
	EndIf

	Dim As Integer hh, mm, ss
	Dim As Double time_decimal

	time_from_str(@hh, @mm, @ss, @as_file_in.time_ )
	time_decimal = time_2decimal(hh, mm, ss)

	time_d(t) = time_decimal

	file = FreeFile
	Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"T.txt" For Append As #file
	Print #file, Using "###.####"; time_decimal
	Close #file

	If isSpectrum <> 0 Then
		fileSpectrumBefore = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/SP.BEFORE."+ext+".txt" For Output As #fileSpectrumBefore
		Print #fileSpectrumBefore, Using "########  "; 0;
		For i = 0 To 10001-1
			Print #fileSpectrumBefore, Using "###### "; i-5000;
		Next
		Print #fileSpectrumBefore,
	EndIf

	If (isSpectrum <> 0) And (isSpectrumMod <> 0) Then
		fileSpectrumAfter = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/SP.AFTER."+ext+".txt" For Output As #fileSpectrumAfter
		Print #fileSpectrumAfter, Using "########  "; 0;
		For i = 0 To 10001-1
			Print #fileSpectrumAfter, Using "###### "; i-5000;
		Next
		Print #fileSpectrumAfter,
	EndIf

	If isAcf <> 0 Then
		fileRcosBefore = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/Rcos.BEFORE."+ext+".txt" For Output As #fileRcosBefore
		Print #fileRcosBefore, Using "########  "; 0;
		For i = 0 To 18
			Print #fileRcosBefore, Using "####.### "; i*DELTA_TAU*1e6;
		Next
		Print #fileRcosBefore,

		fileRsinBefore = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/Rsin.BEFORE."+ext+".txt" For Output As #fileRsinBefore
		Print #fileRsinBefore, Using "########  "; 0;
		For i = 0 To 18
			Print #fileRsinBefore, Using "####.### "; i*DELTA_TAU*1e6;
		Next
		Print #fileRsinBefore,
	EndIf


	If ( isAcf <> 0 ) And (isSpectrumMod <> 0) And (isSpectrum <> 0) Then
		fileRcosAfter = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/Rcos.AFTER."+ext+".txt" For Output As #fileRcosAfter
		Print #fileRcosAfter, Using "########  "; 0;
		For i = 0 To 18
			Print #fileRcosAfter, Using "####.### "; i*DELTA_TAU*1e6;
		Next
		Print #fileRcosAfter,

		fileRsinAfter = FreeFile()
		Open SEANS_DIR_OUT+DirectoryOutput+"/step4/Rsin.BEFORE."+ext+".txt" For Output As #fileRsinAfter
		Print #fileRsinAfter, Using "########  "; 0;
		For i = 0 To 18
			Print #fileRsinAfter, Using "####.### "; i*DELTA_TAU*1e6;
		Next
		Print #fileRsinAfter,
	EndIf


	For h = Hmin To Hmax Step Hstep ' по высоте


		If isAcf <> 0 Then

			Print #fileRcosBefore, Using "########  "; CInt(Hkm(h));
			For i = 0 To 18
				Print #fileRcosBefore, Using "##.##### "; as_file_in.acf[h].rc(i)/as_file_in.acf[h].rc(0);
			Next
			Print #fileRcosBefore,

			Print #fileRsinBefore, Using "########  "; CInt(Hkm(h));
			For i = 0 To 18
				Print #fileRsinBefore, Using "##.##### "; as_file_in.acf[h].rs(i)/as_file_in.acf[h].rc(0);
			Next
			Print #fileRsinBefore,

		EndIf

		If isSpectrum <> 0 Then

			For i = 0 To 550
				r_cos(i) = array_linear_d(i*DELTA_TAU_INTERPOLATED, @x(0), @as_file_in.acf[h].rc(0), 19)
				r_sin(i) = array_linear_d(i*DELTA_TAU_INTERPOLATED, @x(0), @as_file_in.acf[h].rs(0), 19)
			Next i

			fourier_get_spectrum_from_acf(@r_cos(0), @r_sin(0), 550, DELTA_TAU_INTERPOLATED, @sp(0), 10001)

			Print #fileSpectrumBefore, Using "########  "; CInt(Hkm(h));
			For i = 0 To 10001-1
				Print #fileSpectrumBefore, Using "##.### "; sp(i);
			Next
			Print #fileSpectrumBefore,


			If isSpectrumMod <> 0 Then

				For f As Integer = 0 To 10001-1
					If sp(f) > 0.8 Then
						sp(f) = 0.8
					EndIf
					sp(f) -= 0.2
					If sp(f) < 0 Then
						sp(f) = 0
					EndIf
				Next

				Print #fileSpectrumAfter, Using "########  "; CInt(Hkm(h));
				For i = 0 To 10001-1
					Print #fileSpectrumAfter, Using "##.### "; sp(i);
				Next
				Print #fileSpectrumAfter,

				If isVelocityTrap <> 0 Then

					Dim As Double fMinTop, fMaxTop, fMinBottom, fMaxBottom
					For f As Integer = 0 To 10001-1
						If sp(f) > 0.01 Then
							fMinBottom = f
							Exit For
						EndIf
					Next
					For f As Integer = 10001-1 To 0 Step -1
						If sp(f) > 0.01 Then
							fMaxBottom = f
							Exit For
						EndIf
					Next
					For f As Integer = (fMaxBottom+fMinBottom)/2 To 10001-1
						If sp((fMaxBottom+fMinBottom)/2)-sp(f) > 0.01 Then
							fMaxTop = f
							Exit For
						EndIf
					Next
					For f As Integer = (fMaxBottom+fMinBottom)/2 To 0 Step -1
						If sp((fMaxBottom+fMinBottom)/2)-sp(f) > 0.01 Then
							fMinTop = f
							Exit For
						EndIf
					Next

					drift_d_trap(h, t) = ((fMaxBottom+fMinBottom)/2-5000 + (fMaxTop+fMinTop)/2-5000 ) /2 * LAMBDA /2

				EndIf

				fourier_get_acf_from_spectrum(@sp(0), 10001, @as_file_in.acf[h].rc(0), @as_file_in.acf[h].rs(0), 19, DELTA_TAU)

			EndIf

		EndIf

		If ( isAcf <> 0 ) And (isSpectrumMod <> 0) And (isSpectrum <> 0) Then

			Print #fileRcosAfter, Using "########  "; CInt(Hkm(h));
			For i = 0 To 18
				Print #fileRcosAfter, Using "##.##### "; as_file_in.acf[h].rc(i)/as_file_in.acf[h].rc(0);
			Next
			Print #fileRcosAfter,

			Print #fileRsinAfter, Using "########  "; CInt(Hkm(h));
			For i = 0 To 18
				Print #fileRsinAfter, Using "##.##### "; as_file_in.acf[h].rs(i)/as_file_in.acf[h].rc(0);
			Next
			Print #fileRsinAfter,

		EndIf

		Select case num_algo

			Case 1

				Dim As Double c ' количество слагаемых
				c = 0
				drift_d(h, t) = 0
				For tau = kMin To kMax
					If Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit Then
						drift_d(h, t) += Atn( as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) ) / (DELTA_TAU * tau)
						c += 1
					EndIf
				Next tau

				If c <> 0 Then
					drift_d(h, t)  *= -LAMBDA/(4*M_PI)/c
				Else
					drift_d(h, t) = 0
				EndIf

			Case 2

				For tau = kMin To kMax
					r2(tau) = (as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0))^2 + (as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(0))^2
				Next tau

				Dim As Double c, z ' числитель и знаменатель

				c = 0
				For tau = kMin To kMax
					If Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit Then
						c += Atn( as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) ) * r2(tau) * DELTA_TAU * tau
					EndIf
				Next tau

				z = 0
				For tau = kMin To kMax
					If Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit Then
						z += r2(tau) * (DELTA_TAU * tau)^2
					EndIf
				Next tau

				drift_d(h, t) = -LAMBDA/(4*M_PI)*c/z

			Case 3

				Dim As Double c ' количество слагаемых
				c = 0
				drift_d(h, t) = 0
				For tau = kMin To kMax-1
					If (Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit) And (Abs(as_file_in.acf[h].rc(tau+1)/as_file_in.acf[h].rc(0)) > limit) Then
						drift_d(h, t) += Atn( as_file_in.acf[h].rs(tau+1)/as_file_in.acf[h].rc(tau+1) ) - Atn( as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) )
						c += 1
					EndIf
				Next tau

				If c <> 0 Then
					drift_d(h, t) *= -LAMBDA/(4*M_PI*DELTA_TAU*c)
				Else
					drift_d(h, t) = 0
				EndIf

			Case 4

				Dim As Double L = 1e200
				For v As Double = -200 To 200 Step 0.1

					Dim As Double delta = 0
					For tau = kMin To kMax-1
						If Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit Then
							delta += (as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau) - Sin(-4*M_PI*DELTA_TAU*tau/LAMBDA*v)/Cos(4*M_PI*DELTA_TAU*tau/LAMBDA*v))^2
						EndIf
					Next tau

					If delta < L Then
						L = delta
						drift_d(h, t) = v
					EndIf

				Next v

			Case 5

				Dim As Double L = 1e200
				For v As Double = -200 To 200 Step 0.1

					Dim As Double delta = 0
					For tau = kMin To kMax-1
						If Abs(as_file_in.acf[h].rc(tau)/as_file_in.acf[h].rc(0)) > limit Then
							delta += (Atn(as_file_in.acf[h].rs(tau)/as_file_in.acf[h].rc(tau)) - (-4*M_PI*DELTA_TAU*tau/LAMBDA*v))^2
						EndIf
					Next tau

					If delta < L Then
						L = delta
						drift_d(h, t) = v
					EndIf

				Next v

		End Select

	Next h

	If isSpectrum <> 0 Then
		Close #fileSpectrumBefore
	EndIf

	If (isSpectrum <> 0) And (isSpectrumMod <> 0) Then
		Close #fileSpectrumAfter
	EndIf

	If isAcf <> 0 Then
		Close #fileRcosBefore
		Close #fileRsinBefore
	EndIf

	If ( isAcf <> 0 ) And (isSpectrumMod <> 0) And (isSpectrum <> 0) Then
		Close #fileRcosAfter
		Close #fileRsinAfter
	EndIf

	as_file_close(@as_file_in)

Next t

Print_process_percent(1000)
Print "OK"








' Вывод результатов в файлы
Print "Вывод результатов в файлы... ";

file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"H.txt" For Output As #file
For h = Hmin To Hmax Step Hstep ' по высоте
	Print #file, Str(CInt(Hkm(h)))
Next h
Close #file

' Ограничения скорости снизу и сверху
For h = Hmin To Hmax Step Hstep ' по высоте
	For t = 0 To seans_num_in-1
		If drift_d(h, t) > limit_vz Then
			drift_d(h, t) = limit_vz
		EndIf
		If drift_d(h, t) < -limit_vz Then
			drift_d(h, t) = -limit_vz
		EndIf
	Next t
Next h


For h = Hmin To Hmax Step Hstep ' по высоте

	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step4/V."+ Str(CInt(Hkm(h))) +".txt" For Output As #file

	For t = 0 To seans_num_in-1

		Print #file, Using "#####.###  "; drift_d(h, t)
	Next t

	Close #file

Next h




file = FreeFile()
Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"V.txt"  For Output As #file
If Err() <> 0 Then
	break
EndIf

Print #file, "      0 ";

For h = Hmin To Hmax Step Hstep
	Print #file, Using "########  "; CInt(Hkm(h));
Next h

For t As Integer = 0 To seans_num_in-1
	Print #file,
	Print #file, Using "##.#### "; time_d(t);
	For h = Hmin To Hmax Step Hstep
		Print #file, Using "#####.##  "; drift_d(h, t);
	Next h
Next t

Close #file



If (isVelocityTrap <> 0) And (isSpectrumMod <> 0) And (isSpectrum <> 0)  Then

	' Ограничения скорости снизу и сверху
	For h = Hmin To Hmax Step Hstep ' по высоте
		For t = 0 To seans_num_in-1
			If drift_d_trap(h, t) > 200.0 Then
				drift_d_trap(h, t) = 200.0
			EndIf
			If drift_d_trap(h, t) < -200.0 Then
				drift_d_trap(h, t) = -200.0
			EndIf
		Next t
	Next h


	file = FreeFile()
	Open SEANS_DIR_OUT + DirectoryOutput+"/step4/"+"V.Trap.txt"  For Output As #file
	If Err() <> 0 Then
		break
	EndIf

	Print #file, "      0 ";

	For h = Hmin To Hmax Step Hstep
		Print #file, Using "########  "; CInt(Hkm(h));
	Next h

	For t As Integer = 0 To seans_num_in-1
		Print #file,
		Print #file, Using "##.#### "; time_d(t);
		For h = Hmin To Hmax Step Hstep
			Print #file, Using "#####.##  "; drift_d_trap(h, t);
		Next h
	Next t

	Close #file

EndIf


Print "OK"










Print
Print "OK"
break

'''==============================================
