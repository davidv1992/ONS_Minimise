#Configuration
ONS_PATH := /home/david/MFOCSstage/software/OrbitSets

#Flags
CXXFLAGS := -O3 -std=c++11 -g

INCLUDES := -I$(ONS_PATH)/include

GENAUTOMATON_OBJS := GenAutomaton.o
MINIMIZETEST_OBJS := MinimizeTest.o
ISCONNECTED_OBJS := IsConnected.o
FIFO_OBJS := fifo.o
RUNNING_OBJS := running.o

.PHONY: all clean

#Targets
all: GenAutomaton MinimizeTest IsConnected fifo running

GenAutomaton: $(GENAUTOMATON_OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o GenAutomaton $(GENAUTOMATON_OBJS)

MinimizeTest: $(MINIMIZETEST_OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o MinimizeTest $(MINIMIZETEST_OBJS)

IsConnected: $(ISCONNECTED_OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o IsConnected $(ISCONNECTED_OBJS)

fifo: $(FIFO_OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o fifo $(FIFO_OBJS)

running: $(RUNNING_OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o running $(RUNNING_OBJS)

clean:
	rm -f $(GENAUTOMATON_OBJS)
	rm -f $(GENAUTOMATON_OBJS:.o=.d)
	rm -f $(MINIMIZETEST_OBJS)
	rm -f $(MINIMIZETEST_OBJS:.o=.d)
	rm -f $(ISCONNECTED_OBJS)
	rm -f $(ISCONNECTED_OBJS:.o=.d)
	rm -f $(FIFO_OBJS)
	rm -f $(FIFO_OBJS:.o=.d)
	rm -f $(RUNNING_OBJS)
	rm -f $(RUNNING_OBJS:.o=.d)
	rm -f GenAutomaton MinimizeTest IsConnected fifo running

#Common rules
.cpp.o:
	$(CXX) -MD -c $< -o $@ $(INCLUDES) $(CXXFLAGS) $(CPPFLAGS)

#Handle dependency files
-include $(GENAUTOMATON_OBJS:.o=.d)
-include $(MINIMIZETEST_OBJS:.o=.d)
-include $(ISCONNECTED_OBJS:.o=.d)
-include $(FIFO_OBJS:.o=.d)
-include $(RUNNING_OBJS:.o=.d)
