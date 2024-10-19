.PHONY: all clean

all: qp
	- ros build qp.ros

clean:
	- rm -f ./qp

