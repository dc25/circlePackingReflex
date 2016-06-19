{-# Language RecursiveDo #-}
import Optimisation.CirclePacking
import Reflex.Dom
import Data.Map (fromList)


width  = 400
height = 400

svgNamespace = Just "http://www.w3.org/2000/svg"

showCircle :: MonadWidget t m => ((String,Double), (Double,Double)) -> m ()
showCircle ((color, radius), (x, y)) = do

    let circleAttrs = fromList [ ( "cx", show x)
                               , ( "cy", show y)
                               , ( "r",  show radius)
                               , ( "style",  "fill:" ++ color) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()

    return ()

main :: IO()
main = mainWidget $ do

    let svgAttrs = fromList [ ( "viewBox" ,           show (-width / 2) 
                                                    ++ " " ++ show (-height / 2) 
                                                    ++ " " ++ show width 
                                                    ++ " " ++ show height)
                                    , ( "width" , show width)
                                    , ( "height" , show height) 
                                    ] 
        colors = ["Green","Silver", "Lime", "Gray", "Olive", "Yellow", "Maroon", "Navy", "Red", "Blue", "Purple", "Teal", "Fuchsia", "Aqua"]

        stringToCircle = mapM showCircle
                                    . packCircles snd
                                    . zip (cycle colors)
                                    . map read
                                    . words


    rec
        let dString = value ti
        dCircles <- mapDyn stringToCircle dString
        elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ dyn dCircles
        el "br" $ return ()
        ti <- textInput def
    return ()
