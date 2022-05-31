import { useEffect, useState } from "react";
import { Service } from "./types/Service";

const useGaiaService = () => {
  const [result, setResult] = useState<Service<Object>>({
    status: "loading"
  });

  useEffect(() => {
    fetch(
      "/api"
    )
      .then((response) => console.log(response))
      // .then((response) =>
      //   setResult({ status: "loaded", payload: response })
      // )
      // .catch((error) => setResult({ status: "error", error }));
  }, []);

  return result;
};

export default useGaiaService;