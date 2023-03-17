package flash.text {
    import flash.display.DisplayObject;
    
    [Ruffle(InstanceAllocator)]
    public class StaticText extends DisplayObject {
        public native function get text():String;
    }
}