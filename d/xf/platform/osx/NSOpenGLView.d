module xf.platform.osx.NSOpenGLView;

import xf.platform.osx.objc_class;
import xf.platform.osx.objc_runtime;
import xf.platform.osx.NSObject;
import xf.platform.osx.NSGeometry;

class NSOpenGLView {

	this(id window, NSRect rect, id PF) {
		_meta = lookUp("NSOpenGLView");
		_obj = createInstance(meta);
		selDraw = sel("drawDogView");
		selFlush = sel("flushBuffer");

		send(obj, sel("initWithFrame:pixelFormat:"), rect, PF);
		send(window, sel("setContentView:"), obj);

		_context = send(obj, sel("openGLContext"));
		send(context, sel("setView:"), obj);
		send(context, sel("makeCurrentContext"));
	}

	void destroy() {
		send(obj, sel("clearGLContext"));
	}

	void flush() {
		send(context, selFlush);
	}

	id context() {
		return _context;
	}

	private {
		id _context;

		SEL selDraw;
		SEL selFlush;
	}
	mixin NSObject;
}

