import axios from "axios";

export function fetchCount(amount = 1) {
    return new Promise<{ data: number }>((resolve) =>
      setTimeout(() => resolve({ data: amount }), 500)
    );
  }
  
export function fetchAll() {
    async function fetchThis() {
      try {
        const response = await axios.get('https://dog.ceo/api/breeds/image/random')
        console.log(response);
      } catch (error) {
        console.error(error);
      }
    }
    // return new Promise<{data: string}>((resolve) => 
    //   setTimeout(() => resolve({data: 'hello'}), 500)
    // );
}