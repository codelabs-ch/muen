with Muchannel;

pragma Elaborate_All (Muchannel);

package Muenblock.Response_Channel is new Muchannel
   (Element_Type => Block_Response_Type,
    Elements     => Response_Channel_Elements,
    Null_Element => Null_Response,
    Protocol     => Protocol);
