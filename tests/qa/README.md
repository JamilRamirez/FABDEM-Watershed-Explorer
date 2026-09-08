# QA funcional de delimitacion

Este directorio registra pruebas manuales de delimitacion sobre casos reales. Complementa el CI sintetico: no reemplaza las regresiones automatizadas, sino que documenta como responde la aplicacion frente a rios, quebradas, confluencias, cauces anchos y cuencas de distinta escala.

## Protocolo por caso

1. Registrar el punto clicado (lon/lat) y el nombre del cauce o lugar.
2. Anotar el comportamiento esperado antes de delimitar.
3. Ejecutar la delimitacion en la version desplegada.
4. Registrar, cuando esten disponibles: modo de snap, distancia de snap, radio efectivo, outlet final, area de cuenca, motor de trace y tiempo total.
5. Evaluar dos condiciones separadas:
   - `GEOMETRY_CONTINUOUS`: el limite final es una sola geometria continua, sin cuadrados o componentes aislados.
   - `HYDROLOGICALLY_REASONABLE`: el outlet y la cuenca corresponden al cauce que se intento seleccionar.
6. Usar estados:
   - `PASS`: caso reproducible y satisfactorio.
   - `PASS_PROVISIONAL`: visualmente satisfactorio, pero faltan coordenadas o metricas para repetirlo exactamente.
   - `FAIL`: comportamiento incorrecto confirmado.
   - `PENDING_RETEST`: caso conocido que debe repetirse despues de un cambio.
   - `AMBIGUOUS_EXPECTED`: la aplicacion rechaza correctamente un clic cercano a dos cauces equivalentes y pide mayor precision.

## Matriz minima antes de promocion publica

Conviene cubrir al menos:

- quebrada pequena aislada;
- quebrada inmediatamente junto a un rio mayor;
- rio principal estrecho;
- rio principal ancho con islas o barras;
- confluencia;
- cuenca pequena cercana al minimo soportado;
- cuenca mediana;
- cuenca grande;
- cuenca transnacional;
- caso con conexiones D8 diagonales en el limite;
- punto deliberadamente alejado de cualquier cauce, que debe fallar de forma controlada.

El archivo `delineation_smoke_registry.csv` es la fuente de verdad para los resultados manuales.
