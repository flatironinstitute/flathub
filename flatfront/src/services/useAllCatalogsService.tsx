import { useEffect, useState } from 'react';
import { Service } from '../types/Service';
import { CatalogsProps } from '../types/Catalog';

const useAllCatalogsService = () => {
  const [result, setResult] = useState<Service<Object>>({
    status: 'loading',
  });

  useEffect(() => {
    fetch('http://localhost:8092/api/')
      .then((response) => response.json())
      .then((response) => setResult({ status: 'loaded', payload: response }))
      .catch((error) => setResult({ status: 'error', error }));
  }, []);

  return result;
};

export default useAllCatalogsService;
