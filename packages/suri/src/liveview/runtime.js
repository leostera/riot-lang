// LiveView JavaScript Runtime
// Handles WebSocket connection and DOM updates for server-rendered components

class LiveView {
  constructor(elementId, path) {
    this.elementId = elementId;
    this.path = path;
    this.element = null;
    this.socket = null;
    this.connected = false;
  }

  connect() {
    this.element = document.getElementById(this.elementId);
    if (!this.element) {
      console.error(`Element with id "${this.elementId}" not found`);
      return;
    }

    const protocol = window.location.protocol.replace('http', 'ws');
    const url = `${protocol}//${window.location.host}${this.path}`;
    
    this.socket = new WebSocket(url);
    
    this.socket.addEventListener('open', () => {
      this.connected = true;
      console.log('LiveView connected');
      this.mount();
    });

    this.socket.addEventListener('message', (event) => {
      this.handleMessage(event);
    });

    this.socket.addEventListener('close', () => {
      this.connected = false;
      console.log('LiveView disconnected');
      setTimeout(() => this.connect(), 1000);
    });

    this.socket.addEventListener('error', (error) => {
      console.error('LiveView error:', error);
    });
  }

  mount() {
    const data = '"Mount"';
    this.socket.send(data);
  }

  handleMessage(event) {
    try {
      const json = JSON.parse(event.data);
      
      if (json.Patch) {
        const html = json.Patch[0];
        this.patch(html);
      }
    } catch (error) {
      console.error('Failed to handle message:', error);
    }
  }

  patch(html) {
    this.element.innerHTML = html;
    this.rebindEventHandlers();
  }

  rebindEventHandlers() {
    const elements = this.element.querySelectorAll('[data-liveview-id]');
    
    elements.forEach((el) => {
      const id = el.getAttribute('data-liveview-id');
      const eventName = el.getAttribute('data-lv-event');
      
      if (eventName) {
        el.addEventListener(eventName, (e) => {
          this.handleEvent(id, e);
        });
      }
    });
  }

  handleEvent(id, event) {
    const data = JSON.stringify({ Event: [id, ''] });
    this.socket.send(data);
  }

  disconnect() {
    if (this.socket) {
      this.socket.close();
    }
  }
}

// Export for use in browser
window.LiveView = LiveView;

// Convenience function to spawn a new LiveView
window.spawnLiveView = function(elementId, path) {
  const lv = new LiveView(elementId, path);
  lv.connect();
  return lv;
};
