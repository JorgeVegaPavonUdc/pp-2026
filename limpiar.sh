#!/bin/bash

echo "Iniciando limpieza profunda en ~/../PP/estudio..."

# 1. Eliminar archivos de compilación estándar (.cmi, .cmo) en todo el árbol
# Esto limpiará incluso dentro de p12/simplec
find . -type f \( -name "*.cmi" -o -name "*.cmo" \) -delete

# 2. Eliminar ejecutables específicos de la Práctica 1 (nombre, pi, e)
# Basado en los requisitos de los enunciados [1-3]
find . -type f \( -name "nombre" -o -name "pi" -o -name "e" \) -delete

# 3. Eliminar otros ejecutables generados a partir de archivos .ml
# Busca archivos que se llamen igual que un .ml pero sin extensión y que sean ejecutables
# Esto borrará, por ejemplo, el archivo 'def' en la carpeta p3
find . -name "*.ml" -type f | while read -r ml_file; do
    executable="${ml_file%.ml}"
    if [ -f "$executable" ] && [ -x "$executable" ]; then
        rm "$executable"
    fi
done

# 4. Limpieza especial para p12/simplec (si tiene Makefile)
if [ -f "./p12/simplec/Makefile" ]; then
    echo "Ejecutando make clean en p12/simplec..."
    make -C ./p12/simplec clean > /dev/null 2>&1
fi

