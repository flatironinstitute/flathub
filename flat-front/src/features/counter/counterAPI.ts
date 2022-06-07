import axios from 'axios';

export interface Catalog {
  name: string;
  title: string;
  synopsis: string;
  order: string;
};

type GetCatalogsResponse = {
  data: Catalog[];
};

const baseUrl = "http://localhost:8092/"


async function getCatalogs() {
  try {
    // 👇️ const data: GetUsersResponse
    const { data, status } = await axios.get<GetCatalogsResponse>(`${baseUrl}api`);

    console.log(JSON.stringify(data, null, 4));

    // 👇️ "response status is: 200"
    console.log('response status is: ', status);

    return data;
  } catch (error) {
    if (axios.isAxiosError(error)) {
      console.log('error message: ', error.message);
      return error.message;
    } else {
      console.log('unexpected error: ', error);
      return 'An unexpected error occurred';
    }
  }
}

export async function fetchAllCatalogs(){
  const catalogs = await getCatalogs();
  console.log(catalogs);
}



// A mock function to mimic making an async request for data
export function fetchCount(amount = 1) {
  return new Promise<{ data: number }>((resolve) =>
    setTimeout(() => resolve({ data: amount }), 500)
  );
}
