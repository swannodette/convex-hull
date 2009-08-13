import java.util.Random;

public class TimeJarvisMarch {
    public static void main(String[] args) {
	for (int i=0; i<10; i++)
	    test();
    }

    private static void test() {
	final int POINTS = 1000000;

	double x[] = new double[POINTS];
	double y[] = new double[POINTS];
	Random r = new Random();
	for (int i=0; i<POINTS; i++) {
	    x[i] = r.nextGaussian();
	    y[i] = r.nextGaussian();
	}

	JarvisMarch.Points pts = new JarvisMarch.Points(x,y);
	JarvisMarch jm = new JarvisMarch(pts);
	double start = System.currentTimeMillis();
	int n = jm.calculateHull();
	double end = System.currentTimeMillis();
	System.out.printf("%d points found %d vertices %f seconds\n", POINTS, n, (end-start)/1000.);
    }
}