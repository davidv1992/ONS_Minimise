#Configuration
LOIS_PATH := /home/david/MFOCSstage/software/LOIStest/lois

#Mandatory extra flags
CXXFLAGS := -O3 -std=c++11 -g

INCLUDES := -I$(LOIS_PATH)/include

FIFO_OBJS := fifo.o
RUNNING_OBJS := running.o
MINIMIZE_OBJS := minimize.o

.PHONY: all clean

#Targets
all:

fifo: $(FIFO_OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o fifo $(FIFO_OBJS) $(LOIS_PATH)/bin/liblois.a
	
running: $(RUNNING_OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o running $(RUNNING_OBJS) $(LOIS_PATH)/bin/liblois.a

minimize: $(MINIMIZE_OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o minimize $(MINIMIZE_OBJS) $(LOIS_PAHT)/bin/liblois.a

clean:
	rm -f $(FIFO_OBJS)
	rm -f $(FIFO_OBJS:.o=.d)
	rm -f $(RUNNING_OBJS)
	rm -f $(RUNNING_OBJS:.o=.d)
	rm -f $(MINIMIZE_OBJS)
	rm -f $(MINIMIZE_OBJS:.o=.d)
	rm -f fifo running minimize

#Common rules
.cpp.o:
	$(CXX) -MD -c $< -o $@ $(INCLUDES) $(CXXFLAGS) $(CPPFLAGS)

#Handle dependency files
-include $(FIFO_OJBS:.o=.d)
-include $(RUNNING_OJBS:.o=.d)
-include $(MINIMIZE_OBJS:.o=.d)
