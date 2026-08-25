# tick-dynam-seleccion

Simulación en R de un programa de selección recurrente en bovinos combinado con un modelo dinámico de infestación por garrapatas.

## Descripción

Este repositorio reúne funciones para ejecutar simulaciones multianuales de un esquema de mejoramiento genético y reproducción, integrando:

- asignación de animales a campos y regiones,
- generación de progenie con **AlphaSimR**,
- simulación fenotípica,
- modelo dinámico de infestación por garrapatas,
- estimación de EBV mediante BLUP,
- selección de reemplazos machos y hembras para las siguientes rondas de reproducción.

El flujo principal se encuentra en `cruzamiento_sel.R`, donde se coordina todo el proceso año por año.

## Objetivo

El proyecto busca evaluar estrategias de selección genética bajo presión de infestación por garrapatas, observando el impacto de la selección sobre:

- la carga parasitaria,
- los fenotipos simulados,
- el valor genético estimado,
- la genealogía y la composición poblacional en el tiempo.

## Funciones principales

- `cruzamiento_sel()`
  - Orquesta la simulación completa multianual.
  - Genera progenie, simula infestación, calcula EBV y selecciona reemplazos.

- `params_repro()`
  - Calcula y organiza los parámetros reproductivos del sistema.

Además, el repositorio incluye funciones auxiliares para distintas etapas del proceso, por ejemplo:

- `blup_seleccion.R`
- `tickdynam2.R`
- `repro_sel.R`
- `refugo.R`
- `viejos.R`
- `log_txt2.R`
- `logsim2.R`
- `loggarrapatas2.R`
- `salidas_bv.R`

## Requisitos

El proyecto está escrito completamente en **R**. Según el código, utiliza o se apoya en paquetes y herramientas como:

- `data.table`
- `future`
- `parallelly`
- `assertthat`
- **AlphaSimR**
- rutinas de BLUP/selección genómica o fenotípica asociadas al archivo `blup_seleccion.R`

> Nota: si algún paquete adicional es requerido por archivos auxiliares, conviene revisarlos antes de correr la simulación.

## Uso general

1. Cargar las funciones del proyecto.
2. Preparar la población inicial (`pob`) y los parámetros de simulación.
3. Definir el objeto `SimParam` de **AlphaSimR**.
4. Ejecutar `cruzamiento_sel()` con los parámetros reproductivos, genéticos y ambientales correspondientes.

Ejemplo conceptual:

```r
source("params_repro.R")
source("cruzamiento_sel.R")
# source("otros_archivos_auxiliares.R")

param_repro <- params_repro(
  edad_serv = 18,
  npartos = 5,
  pdest = 1,
  nmadres_campo = 100,
  ncampos = 1,
  reposicion_madres = 20,
  porcentaje_padres = 3.5,
  reposicion_padres = 25,
  edad_serv_padres = 24
)

# pob <- ...      # población inicial AlphaSimR
# SP  <- ...      # SimParam de AlphaSimR

resultado <- cruzamiento_sel(
  ncampos = 1,
  h2 = 0.3,
  n_replicas = 1,
  iter = 1,
  naños = 2,
  naños_seleccion = 10,
  donde = "ruta/de/salida",
  pob = pob,
  SP = SP,
  nregiones = 1,
  minimo_a = 0,
  maximo_a = 1,
  size = c(1),
  mu = c(1),
  factor = 1,
  opcion_param = 1,
  crit_sel = "ebv",
  direccion_sel = "neg",
  efectos = NULL,
  options = NULL,
  meanP = 0,
  max_animales = 10000,
  cuando = "1;12"
)
```

## Salidas

La función principal devuelve una lista con resultados como:

- `media_garrapatas`: promedio de infestación por campo y año,
- `conteoinicial`: conteos iniciales de garrapatas,
- `ebv`: tabla final de EBV,
- `fenotipos`: fenotipos acumulados,
- `conteo_ultimo_año`: salida del último año simulado,
- `genea`: información genealógica acumulada.

## Estructura del repositorio

El repositorio está compuesto principalmente por scripts `.R` que dividen la simulación en módulos de reproducción, selección, refugo, fenotipado y cálculo de salidas.

## Licencia

Este proyecto se distribuye bajo la licencia **GNU GPL v3.0**.

## Autor

Repositorio: [SJimmyW/tick-dynam-seleccion](https://github.com/SJimmyW/tick-dynam-seleccion)
