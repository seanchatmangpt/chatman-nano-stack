export const usePipelineWebSocket = () => {
  let socket = null
  let reconnectInterval = null
  const config = useRuntimeConfig()
  
  const connectToPipeline = (callbacks = {}) => {
    socket = new WebSocket(config.public.pipelineWsUrl)
    
    socket.onopen = () => {
      console.log('Connected to pipeline monitor')
      socket.send(JSON.stringify({ 
        type: 'subscribe', 
        channels: ['metrics', 'messages', 'pipeline'] 
      }))
    }
    
    socket.onmessage = (event) => {
      const data = JSON.parse(event.data)
      
      switch (data.type) {
        case 'metrics':
          callbacks.onMetrics?.(data.payload)
          break
        case 'message':
          callbacks.onMessage?.(data.payload)
          break
        case 'pipeline_update':
          callbacks.onPipelineUpdate?.(data.payload)
          break
      }
    }
    
    socket.onerror = (error) => {
      console.error('Pipeline WebSocket error:', error)
    }
    
    socket.onclose = () => {
      console.log('Pipeline connection closed')
      // Auto-reconnect
      if (!reconnectInterval) {
        reconnectInterval = setInterval(() => {
          if (socket.readyState === WebSocket.CLOSED) {
            connectToPipeline(callbacks)
          }
        }, 5000)
      }
    }
  }
  
  const disconnectPipeline = () => {
    if (reconnectInterval) {
      clearInterval(reconnectInterval)
      reconnectInterval = null
    }
    if (socket) {
      socket.close()
      socket = null
    }
  }
  
  const sendCommand = (command, data = {}) => {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ type: 'command', command, data }))
    }
  }
  
  return {
    connectToPipeline,
    disconnectPipeline,
    sendCommand
  }
}