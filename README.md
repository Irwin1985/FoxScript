# FoxScript
Un pequeño lenguaje de programación que vive dentro de Visual Foxpro.

Hace poco estuve mirando el objeto *MSScriptControl.ScriptControl* que viene integrado con Windows y quise hacer algunas pruebas para ver si podía extender las capacidades de Visual Foxpro a través de Scripts en JScript o VBScript pero el resultado no me terminó de convencer así que FoxScript es mi versión de un lenguaje extensible para Visual Foxpro.

# Idea principal
Extender las capacidades de Visual Foxpro a través de pequeños Scripts incrustables en cualquier parte de la aplicación.

# Variable global _CTX
La variable **_CTX** proviene de *Context* y representa al **caller** del script, es decir, desde el script es posible acceder a todos los elementos del contexto. 

### Ejemplo:

Si invocas un script en el `init()` de tu formulario y en tu script ejecutas lo siguiente:

```javascript
_ctx.title = "Nuevo caption desde el Script"
```

Tu formulario modificará su `caption` porque fue actualizado desde el script. La ventaja de este enfoque es poder utilizar tanto las **PEM's** como los cursores no solo del formulario sino del contexto que invoque el script.

Otro objetivo de **FoxScript** es *facilitar* a través de una sintaxis más compacta y amigable la creación de controles y listeners en tiempo de ejecución además de proveer funciones integradas para diversos propósitos.