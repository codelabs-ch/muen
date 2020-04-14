with Muchannel;

pragma Elaborate_All (Muchannel);

package Muenblock.Request_Channel is new Muchannel
   (Element_Type => Block_Request_Type,
    Elements     => Request_Channel_Elements,
    Null_Element => Null_Request,
    Protocol     => Protocol);
