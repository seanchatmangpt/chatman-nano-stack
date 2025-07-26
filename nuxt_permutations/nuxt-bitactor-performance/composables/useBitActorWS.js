export const useBitActorWS = () => {
  const config = useRuntimeConfig()
  let socket = null
  let callbacks = {}
  
  const connectToBitActor = (options = {}) => {
    socket = new WebSocket(config.public.bitactorWsUrl)
    callbacks = options
    
    socket.onopen = () => {
      console.log('Connected to BitActor')
      // Request initial metrics
      socket.send(JSON.stringify({ type: 'subscribe', channel: 'metrics' }))
      socket.send(JSON.stringify({ type: 'subscribe', channel: 'actors' }))
    }
    
    socket.onmessage = (event) => {
      const data = JSON.parse(event.data)
      
      switch (data.type) {
        case 'metrics':
          callbacks.onMetrics?.(data.payload)
          break
        case 'actor_update':
          callbacks.onActorUpdate?.(data.payload)
          break
        case 'message':
          callbacks.onMessage?.(data.payload)
          break
      }
    }
    
    socket.onerror = (error) => {
      console.error('BitActor WebSocket error:', error)
    }
    
    socket.onclose = () => {
      console.log('BitActor connection closed')
      // Attempt reconnection
      setTimeout(() => {
        if (socket.readyState === WebSocket.CLOSED) {
          connectToBitActor(callbacks)
        }
      }, 5000)
    }
  }
  
  const sendCommand = (command, data = {}) => {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ type: 'command', command, data }))
    }
  }
  
  const disconnect = () => {
    if (socket) {
      socket.close()
      socket = null
    }
  }
  
  return {
    connectToBitActor,
    sendCommand,
    disconnect
  }
}