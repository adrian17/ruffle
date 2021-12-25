﻿package {
	import flash.display.SimpleButton;
	import flash.events.Event;
	
	public class ButtonEventWatcher extends SimpleButton {
		public function ButtonEventWatcher() {
			trace("//Constructed ButtonEventWatcher (", this.name, ")!");
			super();
		}
		
		function trace_event(event: Event) {
			trace(this.name + ":" + event);
		}
		
		public function setup() {
			this.addEventListener(Event.ENTER_FRAME, this.trace_event);
			this.addEventListener(Event.EXIT_FRAME, this.trace_event);
			this.addEventListener(Event.ADDED, this.trace_event);
			this.addEventListener(Event.ADDED_TO_STAGE, this.trace_event);
			this.addEventListener(Event.FRAME_CONSTRUCTED, this.trace_event);
			this.addEventListener(Event.REMOVED, this.trace_event);
			this.addEventListener(Event.REMOVED_FROM_STAGE, this.trace_event);
			this.addEventListener(Event.RENDER, this.trace_event);
		}
		
		public function destroy() {
			this.removeEventListener(Event.ENTER_FRAME, this.trace_event);
			this.removeEventListener(Event.EXIT_FRAME, this.trace_event);
			this.removeEventListener(Event.ADDED, this.trace_event);
			this.removeEventListener(Event.ADDED_TO_STAGE, this.trace_event);
			this.removeEventListener(Event.FRAME_CONSTRUCTED, this.trace_event);
			this.removeEventListener(Event.REMOVED, this.trace_event);
			this.removeEventListener(Event.REMOVED_FROM_STAGE, this.trace_event);
			this.removeEventListener(Event.RENDER, this.trace_event);
		}
	}
}