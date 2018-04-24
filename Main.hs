{-# Language RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text, pack, unpack)
import Optimisation.CirclePacking
import Reflex.Dom
import Data.Map (Map, fromList)

packShow :: Show a => a -> Text
packShow = (pack.show)

width  = 400
height = 400

colors :: [String]
colors = ["Green","Silver", "Lime", "Gray", "Olive", "Yellow", "Maroon", "Navy", "Red", "Blue", "Purple", "Teal", "Fuchsia", "Aqua"]

svgNamespace = Just "http://www.w3.org/2000/svg"

showCircle :: MonadWidget t m => ((String,Double), (Double,Double)) -> m ()
showCircle ((color, radius), (x, y)) = do
    let circleAttrs = fromList [ ( "cx", packShow x)
                               , ( "cy", packShow y)
                               , ( "r",  packShow radius)
                               , ( "style",  pack $ "fill:" ++ color) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()

    return ()


svgAttrs :: Map Text Text
svgAttrs = fromList [ ( "viewBox" , pack (
                                        show (-width / 2) 
                                     ++ " " ++ show (-height / 2) 
                                     ++ " " ++ show width 
                                     ++ " " ++ show height) )
                    , ( "width" ,    packShow width)
                    , ( "height" ,   packShow height) 
                    ] 

stringToCircle :: MonadWidget t m => Text -> m [()]
stringToCircle =    mapM showCircle
                  . packCircles snd
                  . zip (cycle colors)
                  . map read
                  . words
                  . unpack

main :: IO()
main = mainWidget $ do
    rec
        let dString = value ti
            dCircles = fmap stringToCircle dString
        elDynAttrNS' svgNamespace "svg" (constDyn svgAttrs) $ dyn dCircles
        el "br" $ return ()
        ti <- textInput $ def { _textInputConfig_initialValue 
                                    = "50 32 30 30 40 35 5 20 43 18 12 " 
                              , _textInputConfig_attributes 
                                    = constDyn $ fromList [("size", "60")]
                              }
    return ()
