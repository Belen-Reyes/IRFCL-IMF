# IRFCL-IMF
Base de datos sobre las Reservas Internacionales y la Liquidez en Moneda Extanjera de los países pertenecientes al FMI.

---- IMF ---- (Última actualización 01-10-2020) <br />
Contiene el código de descarga de la información a través de una API, de esta forma, cada vez que se deseen actualizar los datos, bastará con correr el código de nuevo y volver a guardar los datos.

---- Data ---- <br />
Contiene los datos descaragados en IMF para poder referenciarlos en las funciones correspondientes, en donde: <br />
- Codigos: contiene los códigos y nombres de los paises e indicadores bajo los cuales se descargaron y guardaron.<br />
- Etiquetas: contiene únicamente los códigos descritos arriba.<br />
- Indicadores: Guarda la información segmentada por indicador en caso de querer ejecutar una consulta inmediata de algún indicador de interés.<br />
- Paises: Guarda la información segmentada por cada país en caso de requerir una consulta inmediata de algun país. <br />
- Total: Guarda el total de la información descargada en valores netos y porcentuales. 
