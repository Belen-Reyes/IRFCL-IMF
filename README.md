# IRFCL-IMF
Base de datos sobre las Reservas Internacionales y la Liquidez en Moneda Extanjera de los países pertenecientes al FMI. Última actualización 01-10-2020.

### IMF 
Contiene el código de descarga de la información a través de una API, de esta forma, cada vez que se deseen actualizar los datos, bastará con correr el código de nuevo y volver a guardar los datos.

### Data
Contiene los datos descaragados en IMF para poder referenciarlos en las funciones correspondientes, en donde: <br />
- **Codigos:** contiene los códigos y los nombres de los paises e indicadores bajo los cuales se descargaron y guardaron.<br />
  - Code_C: países.<br />
  - Code_I: indicadores.<br />
  
- **Etiquetas**: contiene únicamente los códigos descritos arriba.<br />

- **Indicadores**: Guarda la información segmentada por indicador en caso de querer ejecutar una consulta inmediata de algún indicador de interés.<br />

- **Paises**: Guarda la información segmentada por cada país en caso de requerir una consulta inmediata de algun país. <br />

- **Total**: Guarda el total de la información descargada en valores netos y porcentuales. <br />
  - TOTAL : Valores netos. <br />
  - TOTAL_P: Valore porcentuales. <br /> 

### Funciones
Contiene las funciones de consulta, gráfica y tabulación de los datos. <br />
- **buscar**: para buscar el código correspondiente de algún país o indicador. Únicamente recibe el nombre del país o del inidcador que se desee buscar `buscar('nombre del país o indicador')`.<br />

- **consulta**: arroja el valor total acumulado de un país repsecto a un indicador entre dos fechas. Recibe los parámetros, país, indicador, fecha_inicio y fecha_fin `consulta('CODE','INDICATOR','YYYY-MM','YYYY-MM')`.<br />

- **graf_uno**: grafica la serie de tiempo de un indicador para un país. Recibe los parámetros indicador y país. `grafica('INDICADOR','PAIS')`.<br />

- **graf_tres**: grafica las series de tiempo de un indicador para tres países. Recibe los parámetros indicador, países (vector) y tipo (total o porcentual) `grafica_tres('INDICADOR','PAIS','tipo: total, porcentaje')`.<br />

- **graf_max**: grafica los tres países con máximos valores en un determinado indicador. Recibe los parámetros indicador y tipo. `graf_max('INDICADOR', 'tipo': total, porcentaje)`.<br />

- **graf_min**: grafica los tres países con mínimos valores en un determinado indicador. Recibe los parámetros indicador y tipo. `graf_min('INDICADOR', 'tipo': total, porcentaje)`.<br />

- **tabla_max**: tabula los cinco países con máximos valores en un determinado indicador. Recibe los parámetros indicador y tipo. `tabla_max('INDICADOR', 'tipo': total, porcentaje)`.<br />

- **tabla_min**: tabula los cinco países con mínimos valores en un determinado indicador. Recibe los parámetros indicador y tipo. `tabla_min('INDICADOR', 'tipo': total, porcentaje)`.<br />

- **tabla_total**: tabula todos los países en orden decreciente para un determinado indicador. Recibe los parámetros indicador y tipo. `tabla_total('INDICADOR', 'tipo': total, porcentaje)`.<br />
