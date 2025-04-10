# Evaluación COBOL - Manejo de Archivos con COMP-3

## Descripción
Este proyecto contiene programas en COBOL para la creación, lectura y filtrado de archivos de empleados utilizando el formato COMP-3.

## Programas de Creación de Archivos
Los siguientes programas generan archivos de entrada con datos de empleados en formato COMP-3:
- **EmployeeCreateFile.cbl**: Crea un archivo con registros de empleados.
- **EMPLOYEE-CREATE-F-1K.cbl**: Genera un archivo con 1,000 registros de empleados en formato COMP-3.

## Programas de Lectura y Filtrado
Los siguientes programas leen el archivo generado y procesan la información para generar reportes:
- **EmployeeReadFileCreateReport.cbl**: Lee el archivo de empleados y genera un informe con los datos procesados.
- **EMPLOYEE-READ-FILE-CREATE-RP1K.cbl**: Filtra los empleados con `type="E"` y genera un nuevo archivo con estos registros.

## Requisitos
- COBOL Compiler compatible con COMP-3 (en este caso se creó con gnuCobol teniendo como SO Ubuntu en WSL).
- Configuración adecuada para la ejecución de programas COBOL en tu entorno.

## Uso
1. Ejecutar los programas de creación para generar el archivo de entrada.
2. Usar los programas de lectura y filtrado para procesar el archivo y obtener los reportes.


## Compilación y Ejecución
Para compilar un programa COBOL, usar el siguiente comando:

```bash
cobc -x [nombre_programa].cbl
```

Para ejecutar el programa compilado:

```bash
./[nombre_programa]
```


