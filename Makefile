web/img:
	mkdir -p web/img && cd web/img && for i in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z Á É Í Ó Ú Ü Ñ; do convert -size 64x64 xc:White -gravity South -pointsize 60 -annotate +0-8 $i $(echo $i | python3 -c 'import sys; print(sys.stdin.read().lower())').png; done && convert -size 64x64 xc:White -gravity South -pointsize 60 -annotate 0 _ _.png && convert -size 32x32 xc:White -gravity South -pointsize 18 -annotate 0 '???' help.png

