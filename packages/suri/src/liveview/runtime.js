// LiveView JavaScript Runtime
// Handles WebSocket connection and DOM updates for server-rendered components

class LiveView {
  constructor(elementId, wsPath) {
    this.elementId = elementId;
    this.wsPath = wsPath;
    this.element = null;
    this.socket = null;
    this.handlers = new Map();
    this.connected = false;
  }
  
  connect() {
    this.element = document.getElementById(this.elementId);
    if (!this.element) {
      console.error(`[LiveView] Element #${this.elementId} not found`);
      return;
    }
    
    // Extract session token from data attribute
    const session = this.element.getAttribute('data-lv-session');
    
    const protocol = window.location.protocol.replace('http', 'ws');
    let url = `${protocol}//${window.location.host}${this.wsPath}`;
    
    // Append session token to URL if present
    if (session) {
      url += `?session=${encodeURIComponent(session)}`;
    }
    
    console.log(`[LiveView] Connecting to ${url}`);
    this.socket = new WebSocket(url);
    
    this.socket.addEventListener('open', () => {
      this.connected = true;
      console.log('[LiveView] Connected');
      this.mount();
    });
    
    this.socket.addEventListener('message', (event) => {
      this.handleMessage(event.data);
    });
    
    this.socket.addEventListener('close', () => {
      this.connected = false;
      console.log('[LiveView] Disconnected, reconnecting in 1s...');
      setTimeout(() => this.connect(), 1000);
    });
    
    this.socket.addEventListener('error', (error) => {
      console.error('[LiveView] WebSocket error:', error);
    });
  }
  
  mount() {
    console.log('[LiveView] Mounting...');
    this.send('"Mount"');
  }
  
  handleMessage(data) {
    try {
      const msg = JSON.parse(data);
      
      if (msg.Patch) {
        console.log('[LiveView] Received patch');
        this.patch(msg.Patch);
      } else if (msg.Error) {
        console.error('[LiveView] Server error:', msg.Error);
      } else {
        console.warn('[LiveView] Unknown message:', msg);
      }
    } catch (error) {
      console.error('[LiveView] Failed to handle message:', error, data);
    }
  }
  
  patch(html) {
    // Full HTML replacement (simple approach)
    this.element.innerHTML = html;
    
    // Rebind event handlers
    this.rebindEventHandlers();
  }
  
  rebindEventHandlers() {
    // Clear old handlers
    this.handlers.forEach(({ element, eventName, listener }) => {
      element.removeEventListener(eventName, listener);
    });
    this.handlers.clear();
    
    // Find all elements with data-lv-handler attribute
    const elements = this.element.querySelectorAll('[data-lv-handler]');
    
    elements.forEach(el => {
      const handlerId = el.getAttribute('data-lv-handler');
      const eventName = el.getAttribute('data-lv-event');
      
      if (!handlerId || !eventName) {
        console.warn('[LiveView] Element missing handler ID or event name', el);
        return;
      }
      
      const listener = (e) => {
        e.preventDefault();
        this.handleEvent(handlerId, e);
      };
      
      el.addEventListener(eventName, listener);
      this.handlers.set(handlerId, { element: el, eventName, listener });
    });
    
    console.log(`[LiveView] Bound ${this.handlers.size} event handlers`);
  }
  
  handleEvent(handlerId, event) {
    const eventData = this.serializeEvent(event);
    
    console.log(`[LiveView] Event: ${handlerId} (${event.type})`);
    
    const msg = JSON.stringify({
      Event: [handlerId, eventData]
    });
    
    this.send(msg);
  }
  
  serializeEvent(event) {
    // Extract relevant event data
    const data = {
      type: event.type,
    };
    
    if (event.target) {
      if (event.target.value !== undefined) {
        data.value = event.target.value;
      }
      if (event.target.checked !== undefined) {
        data.checked = event.target.checked;
      }
      if (event.target.tagName) {
        data.tagName = event.target.tagName;
      }
    }
    
    return JSON.stringify(data);
  }
  
  send(data) {
    if (this.connected && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(data);
    } else {
      console.warn('[LiveView] Cannot send - not connected');
    }
  }
  
  disconnect() {
    if (this.socket) {
      this.socket.close();
    }
  }
}

// Export for use in browser
if (typeof window !== 'undefined') {
  window.LiveView = LiveView;
}

// Convenience function
if (typeof window !== 'undefined') {
  window.spawnLiveView = function(elementId, wsPath) {
    const lv = new LiveView(elementId, wsPath);
    lv.connect();
    return lv;
  };
}
