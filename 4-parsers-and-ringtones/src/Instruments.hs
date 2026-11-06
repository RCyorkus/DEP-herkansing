module Instruments where

import Control.Applicative (liftA2)
import Types

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

silence :: Seconds -> Sound
silence x = floatSound $ replicate (round (sampleRate * x)) (0.0 :: Float)

sine :: Instrument
sine = instrument $ \hz duration -> map (sin . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

noise :: Instrument
noise = instrument $ \_ duration -> map f [0.0..sampleRate * duration - 1000] <> replicate 1000 0
  where f t = let x = min (sin t) (cos t)
                  y = max (300+200*cos t) (300+200*sin t)
                  z = max (300+200*cos(t*1.5)) (300+200*sin (t*1.7))
                  s1 = tan(t*2) * sin(x*y*600) * abs (sin (t/10)) * abs (cos (t/5))
                  s2 = abs (sin (t*10)) * sin (x*y)
                  s3 = sin (sin (z*t*0.1))
              in (s1+s2+s3) / 3

bass :: Instrument
bass = instrument $ \hz duration -> zipWithL (+) (replicate (ceiling (sampleRate * duration)) 0)
                                  . map (sin . (* (hz * pi * 2 / sampleRate))) $ [hz, hz-0.1..0]

triangle :: Float -> Float
triangle x = (2/pi) * asin (sin (pi*x/2))

square :: Float -> Float
square = signum . cos

squareWave :: Instrument
squareWave = instrument $ \hz duration -> map (square . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

triangleWave :: Instrument
triangleWave = instrument $ \hz duration -> map (triangle . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

kick :: Instrument
kick = instrument $ \hz duration -> zipWithL (+) (replicate (ceiling (sampleRate * duration)) 0)
                                  . map (ground . (* (hz * 16 * pi * 2 / sampleRate))) $ map (\e -> 0.996**e * hz) [1..0.5*sampleRate*duration]
  where ground = liftA2 (+) square triangle

attack :: Modifier
attack = modifier $ zipWith (*) . map (min 1.0) $ [0.0,0.001..]

release :: Modifier
release = modifier $ \output -> zipWith (*) output . reverse . take (length output) . map (min 1.0) $ [0.0,0.001..]

distort :: Modifier
distort = modifier $ reverse . zipWith (*) (map (max 0.0) [0.0,0.001..]) . reverse

popRelease :: Modifier
popRelease = modifier $ zipWith (*) . map (max 0.0) $ [1.0,0.999..]

defaultInstrument :: Instrument
defaultInstrument = modifyInstrument sine (attack <> release)

defaultSquare :: Instrument
defaultSquare = modifyInstrument (modifyInstrument squareWave attack) release

defaultTriangle :: Instrument
defaultTriangle = modifyInstrument (modifyInstrument triangleWave attack) release

pop :: Instrument
pop = modifyInstrument sine (attack <> popRelease)

twisted :: Instrument
twisted = modifyInstrument sine (attack <> distort)

pad :: [Note] -> [Note]
pad n = [Pause Quarter] ++ n ++ [Pause Quarter]

generateWave :: Beats -> Instrument -> [Note] -> Sound
generateWave bpm inst = mconcat . map note . pad
  where note :: Note -> Sound
        note (Note tone oct dur) = arrange inst (pitch tone oct) (beats dur * 240/bpm)
        note (Pause dur) = floatSound $ replicate (ceiling $ sampleRate * beats dur * 240/bpm) 0

toFloat :: Enum a => a -> Float
toFloat = fromIntegral . fromEnum

pitch :: Tone -> Octave -> Hz
pitch tone oct = ((2**) . subtract 4 $ toFloat oct) * freq (toFloat tone)
  where freq :: Semitones -> Hz -- [https://pages.mtu.edu/~suits/NoteFreqCalcs.html]
        freq n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

beats :: Duration -> Float
beats Full = 1
beats Half = 1/2
beats Quarter = 1/4
beats Eighth = 1/8
beats Sixteenth = 1/16
beats Thirtysecond = 1/32
beats (Dotted d) = 3/2 * beats d