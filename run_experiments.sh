#!/bin/bash
set -e

#RUN without SMT dump

IS_CONCURRENT=true

AMP='&'
if ! $IS_CONCURRENT
then
  AMP=''
fi

#Compile
stack build

#Run minimal libraries first

#delete old files first and then run

#new-min-blinky
rm -f new-min-blinky.svg new-min-blinky.edg new-min-blinky.out

eval "stack exec edg-prototype -- new-min-blinky -g new-min-blinky.svg  -o new-min-blinky.edg >>new-min-blinky.out 2>&1" $AMP

#new-min-datalogger
rm -f new-min-datalogger.svg new-min-datalogger.edg new-min-datalogger.out

eval "stack exec edg-prototype -- new-min-datalogger -g new-min-datalogger.svg  -o new-min-datalogger.edg >>new-min-datalogger.out 2>&1" $AMP

#new-min-ol-datalogger
rm -f new-min-ol-datalogger.svg new-min-ol-datalogger.edg new-min-ol-datalogger.out

eval "stack exec edg-prototype -- new-min-ol-datalogger -g new-min-ol-datalogger.svg  -o new-min-ol-datalogger.edg >>new-min-ol-datalogger.out 2>&1" $AMP

#new-min-feedbackfan
rm -f new-min-feedbackfan.svg new-min-feedbackfan.edg new-min-feedbackfan.out

eval "stack exec edg-prototype -- new-min-feedbackfan -g new-min-feedbackfan.svg  -o new-min-feedbackfan.edg >>new-min-feedbackfan.out 2>&1" $AMP

#new-min-robot
rm -f new-min-robot.svg new-min-robot.edg new-min-robot.out

eval "stack exec edg-prototype -- new-min-robot -g new-min-robot.svg  -o new-min-robot.edg >>new-min-robot.out 2>&1" $AMP

#new-min-simon
rm -f new-min-simon.svg new-min-simon.edg new-min-simon.out

eval "stack exec edg-prototype -- new-min-simon -g new-min-simon.svg  -o new-min-simon.edg >>new-min-simon.out 2>&1" $AMP

#new-min-simon-trinket
rm -f new-min-simon-trinket.svg new-min-simon-trinket.edg new-min-simon-trinket.out

eval "stack exec edg-prototype -- new-min-simon-trinket -g new-min-simon-trinket.svg  -o new-min-simon-trinket.edg >>new-min-simon-trinket.out 2>&1" $AMP

#new-min-alt-simon-trinket
rm -f new-min-alt-simon-trinket.svg new-min-alt-simon-trinket.edg new-min-alt-simon-trinket.out

eval "stack exec edg-prototype -- new-min-alt-simon-trinket -g new-min-alt-simon-trinket.svg  -o new-min-alt-simon-trinket.edg >>new-min-alt-simon-trinket.out 2>&1" $AMP


#medium libraries runs
#delete old files first and then run

#new-med-blinky
rm -f new-med-blinky.svg new-med-blinky.edg new-med-blinky.out

eval "stack exec edg-prototype -- new-med-blinky -g new-med-blinky.svg  -o new-med-blinky.edg >>new-med-blinky.out 2>&1" $AMP

#new-med-datalogger
rm -f new-med-datalogger.svg new-med-datalogger.edg new-med-datalogger.out

eval "stack exec edg-prototype -- new-med-datalogger -g new-med-datalogger.svg  -o new-med-datalogger.edg >>new-med-datalogger.out 2>&1" $AMP

#new-med-ol-datalogger
rm -f new-med-ol-datalogger.svg new-med-ol-datalogger.edg new-med-ol-datalogger.out

eval "stack exec edg-prototype -- new-med-ol-datalogger -g new-med-ol-datalogger.svg  -o new-med-ol-datalogger.edg >>new-med-ol-datalogger.out 2>&1" $AMP

#new-med-feedbackfan
rm -f new-med-feedbackfan.svg new-med-feedbackfan.edg new-med-feedbackfan.out

eval "stack exec edg-prototype -- new-med-feedbackfan -g new-med-feedbackfan.svg  -o new-med-feedbackfan.edg >>new-med-feedbackfan.out 2>&1" $AMP

#new-med-robot
rm -f new-med-robot.svg new-med-robot.edg new-med-robot.out

eval "stack exec edg-prototype -- new-med-robot -g new-med-robot.svg  -o new-med-robot.edg >>new-med-robot.out 2>&1" $AMP

#new-med-simon
rm -f new-med-simon.svg new-med-simon.edg new-med-simon.out

eval "stack exec edg-prototype -- new-med-simon -g new-med-simon.svg  -o new-med-simon.edg >>new-med-simon.out 2>&1" $AMP

#new-med-simon-trinket
rm -f new-med-simon-trinket.svg new-med-simon-trinket.edg new-med-simon-trinket.out

eval "stack exec edg-prototype -- new-med-simon-trinket -g new-med-simon-trinket.svg  -o new-med-simon-trinket.edg >>new-med-simon-trinket.out 2>&1" $AMP

#new-med-alt-simon-trinket
rm -f new-med-alt-simon-trinket.svg new-med-alt-simon-trinket.edg new-med-alt-simon-trinket.out

eval "stack exec edg-prototype -- new-med-alt-simon-trinket -g new-med-alt-simon-trinket.svg  -o new-med-alt-simon-trinket.edg >>new-med-alt-simon-trinket.out 2>&1" $AMP

#full size libraries runs
#delete old files first and then run

#new-blinky
rm -f new-blinky.svg new-blinky.edg new-blinky.out

eval "stack exec edg-prototype -- new-blinky -g new-blinky.svg  -o new-blinky.edg >>new-blinky.out 2>&1" $AMP

#new-datalogger
rm -f new-datalogger.svg new-datalogger.edg new-datalogger.out

eval "stack exec edg-prototype -- new-datalogger -g new-datalogger.svg  -o new-datalogger.edg >>new-datalogger.out 2>&1" $AMP

#new-ol-datalogger
rm -f new-ol-datalogger.svg new-ol-datalogger.edg new-ol-datalogger.out

eval "stack exec edg-prototype -- new-ol-datalogger -g new-ol-datalogger.svg  -o new-ol-datalogger.edg >>new-ol-datalogger.out 2>&1" $AMP

#new-feedbackfan
rm -f new-feedbackfan.svg new-feedbackfan.edg new-feedbackfan.out

eval "stack exec edg-prototype -- new-feedbackfan -g new-feedbackfan.svg  -o new-feedbackfan.edg >>new-feedbackfan.out 2>&1" $AMP

#new-robot
rm -f new-robot.svg new-robot.edg new-robot.out

eval "stack exec edg-prototype -- new-robot -g new-robot.svg  -o new-robot.edg >>new-robot.out 2>&1" $AMP

#new-simon
rm -f new-simon.svg new-simon.edg new-simon.out

eval "stack exec edg-prototype -- new-simon -g new-simon.svg  -o new-simon.edg >>new-simon.out 2>&1" $AMP

#new-simon-trinket
rm -f new-simon-trinket.svg new-simon-trinket.edg new-simon-trinket.out

eval "stack exec edg-prototype -- new-simon-trinket -g new-simon-trinket.svg  -o new-simon-trinket.edg >>new-simon-trinket.out 2>&1" $AMP

#new-alt-simon-trinket
rm -f new-alt-simon-trinket.svg new-alt-simon-trinket.edg new-alt-simon-trinket.out

eval "stack exec edg-prototype -- new-alt-simon-trinket -g new-alt-simon-trinket.svg  -o new-alt-simon-trinket.edg >>new-alt-simon-trinket.out 2>&1" $AMP
