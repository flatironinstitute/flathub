import setuptools

setuptools.setup(
    name="flathub",
    version="0.1",
    author="Dylan Simon, Flatiron Institute",
    author_email="dsimon@flatironinstitute.org",
    description="Client library for flathub.flatironinstitute.org simulation data",
    url="https://github.com/flatironinstitute/flathub",
    packages=setuptools.find_packages(),
    classifiers=(
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 3",
	"License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ),
    install_requires=['numpy']
)
