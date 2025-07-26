export const useAshAPI = () => {
  const { $apollo } = useNuxtApp()
  
  const getResources = async (filters = {}) => {
    const query = gql`
      query GetResources($filters: ResourceFilters) {
        resources(filters: $filters) {
          id
          name
          type
          attributes
          relationships {
            type
            target
          }
          created_at
          updated_at
        }
      }
    `
    
    const { data } = await $apollo.query({
      query,
      variables: { filters }
    })
    
    return data.resources
  }
  
  const createResource = async (input) => {
    const mutation = gql`
      mutation CreateResource($input: ResourceInput!) {
        createResource(input: $input) {
          id
          name
          type
          success
          errors
        }
      }
    `
    
    const { data } = await $apollo.mutate({
      mutation,
      variables: { input }
    })
    
    return data.createResource
  }
  
  const subscribeToUpdates = (callback) => {
    const subscription = gql`
      subscription ResourceUpdates {
        resourceUpdated {
          id
          name
          type
          event
        }
      }
    `
    
    return $apollo.subscribe({ query: subscription }).subscribe(callback)
  }
  
  return {
    getResources,
    createResource,
    subscribeToUpdates
  }
}