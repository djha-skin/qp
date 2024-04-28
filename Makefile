.PHONY: all clean

all: qp

qp: com.djhaskin.qp.asd src/*.lisp qp.ros
	- ros build qp.ros

clean:
	- rm -f qp