# GeoSPARQL_TFM
Archivos generados para la asignación automática de parcelas del IFN3 a municipios mediante Linked Open Data 

## Datos
Contiene las siguientes carpetas:
- **Asignacion_parcela_muni**: contiene los resultados de las asignaciones en formato Turtle (Fuseki) y Shapefile (QGIS) de las parcelas del IFN3 al código INE del municipio en el que se encuentran. 
- **CSVs**: contiene distintos CSVs de datos descargados del repositorio Cross-Forest, del IFN3 y datos generados.
- **Descarga_Croos_Forest**: archivos Turtle cargados en los entornos de pruebas.
- **Recintos_municipales**: contiene todos los recintos municipales españoles en formato Shapefile, GeoJSON y Turtle. 
- **SHP_Parcelas_IFN3**: contiene un archivo de puntos con las localizaciones de las parcelas del IFN3 en formato Shapefile. 

## Mapas
Contiene las siguientes carpetas con Shapefiles que representan las especies dominantes de los municipios según su área basimétrica media máxima (G) y el número medio de pies/ha(N), tomando en cuenta datos del IFN3, para:
- **España**  
- **Sierra de la Demanda**

## Scripts
- **Check_digit**: Script en RStudio para generar los dígitos de control.
- **Compara_muni_inventario**: Script en RStudio utilizado para comparar los resultados de las asignaciones de parcela a municipio.
- **Mapas_SP_dominante**: Script en RStudio para generar los mapas de las especies dominantes por municipio.
- **Asignacion_parcela_municipio**: scripts para configurar los entornos de pruebas con Virtuoso y Fuseki, script para la asignación automática con QGIS, scripts para la descarga de los datos del repositorio Cross-Forest.
- **Conversion_municipios_RDF**: consulta SPARQL-Generate para transformar los GeoJSON de los recintos municipales a RDF y script para realizar la transformación de forma automática. 

