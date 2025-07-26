// WebSocket Client for CNS Forge Notifications
// NO TypeScript - Pure JavaScript

class CnsForgeNotificationClient {
  constructor(url = 'ws://localhost:4000/socket/websocket') {
    this.url = url
    this.socket = null
    this.listeners = new Map()
  }
  
  connect() {
    this.socket = new WebSocket(this.url)
    
    this.socket.onopen = () => {
      console.log('🔔 Connected to CNS Forge notifications')
    }
    
    this.socket.onmessage = (event) => {
      const data = JSON.parse(event.data)
      this.handleMessage(data)
    }
  }
  
  handleMessage(data) {
    const { topic, event, payload } = data
    
    switch (event) {
      case 'pipeline_started':
        console.log(`🚀 Pipeline ${payload.pipeline_id} started`)
        break
      case 'stage_completed':
        console.log(`✅ Stage ${payload.stage} completed`)
        break
      case 'pipeline_completed':
        console.log(`🎉 Pipeline ${payload.pipeline_id} completed!`)
        break
    }
  }
}

// Usage
const client = new CnsForgeNotificationClient()
client.connect()
